#### Area calculation based on valid risk level pixels

```
var pixelArea = ee.Image.pixelArea().divide(1e6);  // Convert sqm to sq km
var riskLevels = [-9999, 1, 2, 3];  // Define the list of risk levels present in your model, including "Absent" (i.e., -9999)  
var results = ee.FeatureCollection([]);  // Create an empty FeatureCollection to store the results for each province and risk level
```
##### Step 1: Initialize results collection
```
var totalAreaPerProvince = ee.FeatureCollection([]);  // Store the total area of valid pixels for each province
```

##### Step 2: Iterate over provinces
```
arg_states.aggregate_array('NAME_1').getInfo().forEach(function(provinceName) {
```  
  
##### Step 3: Mask the raster for valid risk levels (including "Absent")
```
var validRiskMask = model.gte(-9999);
```

  // Step 4: Calculate total area for each province (based only on valid pixels)
  var validAreaPerProvince = pixelArea.updateMask(validRiskMask)
    .reduceRegions({
      collection: arg_states.filter(ee.Filter.eq('NAME_1', provinceName)),
      reducer: ee.Reducer.sum(),
      scale: model.projection().nominalScale(),
      tileScale: 16
    })
    .map(function(feature) {
      
      // Get the area in square km
      var totalAreaKm = ee.Number(feature.get('sum'));
      
      return feature.set('Total_Valid_Area_km', totalAreaKm);
    });

  // Add total valid area to the results
  totalAreaPerProvince = totalAreaPerProvince.merge(validAreaPerProvince);
});

// Print to verify total valid area per province
print('Total valid area per province (based on valid risk pixels):', totalAreaPerProvince);

// Step 5: Iterate over risk levels, including -9999 for "Absent"
riskLevels.forEach(function(riskLevel) {
  // Step 6: Iterate over provinces
  arg_states.aggregate_array('NAME_1').getInfo().forEach(function(provinceName) {
    
    // Mask the raster for the current risk level
    var riskMask = model.eq(riskLevel);
    
    // Calculate the area per risk level (in sq km) for valid pixels only
    var areaPerRiskLevel = pixelArea.updateMask(riskMask)
      .reduceRegions({
        collection: arg_states.filter(ee.Filter.eq('NAME_1', provinceName)),
        reducer: ee.Reducer.sum(),
        scale: model.projection().nominalScale(),
        tileScale: 16
      })
      .map(function(feature) {
        // Get the area in square km
        var riskAreaKm = ee.Number(feature.get('sum'));

        // Label "Absent" areas if the risk level is -9999
        var riskLevelLabel = riskLevel === -9999 ? 'Absent' : riskLevel;

        return feature.set('RiskLevel', riskLevelLabel)
                      .set('Risk_Area_km', riskAreaKm);
      });

    // Calculate the number of risk-level pixels in the province
    var riskPixelCount = pixelArea.updateMask(riskMask)
      .reduceRegions({
        collection: arg_states.filter(ee.Filter.eq('NAME_1', provinceName)),
        reducer: ee.Reducer.count(),
        scale: model.projection().nominalScale(),
        tileScale: 16
      })
      .map(function(feature) {
        return feature.set('RiskPixelCount', feature.get('count'));
      });

    // Merge pixel count data into the area calculations
    areaPerRiskLevel = areaPerRiskLevel.map(function(feature) {
      var riskPixelFeature = riskPixelCount.filter(ee.Filter.eq('NAME_1', feature.get('NAME_1'))).first();
      return feature.set('RiskPixelCount', ee.Algorithms.If(riskPixelFeature, riskPixelFeature.get('RiskPixelCount'), 0));
    });

    // Add the area calculations for the current risk level and province to the results
    results = results.merge(areaPerRiskLevel);
  });
});


// Step 6: Calculate the percentage of risk areas (including "Absent") relative to the total valid area
results = results.map(function(feature) {
  var provinceName = feature.get('NAME_1');
  var totalArea = totalAreaPerProvince.filter(ee.Filter.eq('NAME_1', provinceName)).first();
  var totalValidAreaKm = ee.Number(totalArea.get('Total_Valid_Area_km'));
  
  var riskAreaKm = ee.Number(feature.get('Risk_Area_km'));
  var percentage = riskAreaKm.divide(totalValidAreaKm).multiply(100);
  
  return feature.set('Total_Valid_Area_km', totalValidAreaKm)
                .set('Percentage', percentage)
                .set('RiskPixelCount', feature.get('RiskPixelCount'));  // Fixed line
});

// Step 7: Export the results as a CSV file to Google Drive
Export.table.toDrive({
  folder: 'Wild_boar_pig_interface',
  collection: results,
  description: 'Risk_Level_Areas_B',
  fileFormat: 'CSV',
  selectors: ['NAME_1', 'RiskLevel', 'Risk_Area_km', 'RiskPixelCount', 'Total_Valid_Area_km', 'Percentage']  // Choose columns to include
});

