### Google Earth Engine app Developmment
##### The following is the JavaScript code used to build the GEE web application

```r
// Get the projection of the image
var projection = model.projection();

// Get the nominal scale (pixel size) of the image in meters
var scale = projection.nominalScale();

// Print the resolution (pixel size)
print('Pixel size (resolution) in meters:', scale);  // 10,000 m

// Optionally, inspect the projection details
print('Projection details:', projection);
print(model.projection());

// Configure the map with a minimal set of controls
Map.setControlVisibility(true);  // Layers, Mapa, Satélite and drawing options show up 
Map.setControlVisibility({scaleControl: true, zoomControl: true});
Map.style().set({cursor: 'crosshair'});
Map.setOptions('SATELLITE');
Map.setCenter(-58.2, -19.2, 3)

// Map settings
// Set the default basemap to satellite
Map.setOptions('SATELLITE');
Map.setControlVisibility(true);  // Layers, Mapa, Satélite and drawing options show up 
Map.setControlVisibility({scaleControl: true, zoomControl: true});
Map.style().set({cursor: 'crosshair'});

Map.centerObject(arg_states, 5)

// Set up title and summary widgets
var header1 = ui.Label("Risk of interaction between wild boar and domestic pigs in Argentina", 
            {fontSize: '18px', fontWeight: 'bold', color: '4A997E'});
            
// App summary added as different text chunks for beter viewing.

var text1 = ui.Label(
"This tool allows exploring the main results from analyses presented in the publication " +
"\"Let's meet at the interface: modelling the potential interaction between wild boar " +
" and domestic pigs in Argentina.\"",
{fontSize: '14px'});

var text2 = ui.Label(
"Select a State from the drop-down menu and left-click on it to display a graphic with the area occupied by each risk level. ",
{fontSize: '14px'});

//----------------------------------------------------------------------    
// Create a panel 1 to hold the above text
//----------------------------------------------------------------------

var panel1 = ui.Panel({
  widgets:[header1, text1, text2], // Adds header and text
  style:{width: '370px', position:'top-right', backgroundColor: 'white'}});


//-------------------------------------------------------------------------- 
// Create panel 2 to house a line separator
//-------------------------------------------------------------------------- 

var panel2 = ui.Panel([
  ui.Label({
    value: '______________________________________________________',
    style: {fontWeight: 'bold',  color: '4A997E'},
  })]);

// Add this new panel to the larger panel we created 
panel1.add(panel2)

// Add our main panel to the root of our GUI
ui.root.insert(1, panel1)

// Checkbox for activating/deactivating the farms layer
var toggleFarmsCheckbox = ui.Checkbox({
  label: 'Show/Hide farms',
  value: false,  // Default to unchecked (layer off)
  onChange: function(checked) {
    if (checked) {
      // Add farms layer if checked
      Map.add(farmsLayer);
    } else {
      // Remove farms layer if unchecked
      Map.remove(farmsLayer);
    }
  },
  style: {fontWeight: 'bold', fontSize: '14px', margin: '10px'}
});

//------------------------------------------------------
// Add drop-down menu for states
//------------------------------------------------------

// Get the state names and sort them alphabetically
var selectedLayer = null;  // Variable to keep track of the selected state layer

// Get the state names and sort them alphabetically
var items1 = arg_states.aggregate_array('NAME_1');
items1.evaluate(function(List) {
  List.sort();  // Sort the list alphabetically
  List.unshift('None');  // Add "None" at the top of the list
  dropdown1.items().reset(List);  // Reset the dropdown items with the updated list
  dropdown1.setPlaceholder('States');
});

// Vis params for polygons
var vis_params_states = {
    'color': '#a569bd', 
    'width': 3,
    'lineType': 'solid',
    'fillColor': '#00000033',
}

// Define the dropdown
var dropdown1 = ui.Select({
  placeholder: 'Biodiversity hotspots',  // Please wait...
  onChange: function(key){
    if (selectedLayer !== null) {
      Map.remove(selectedLayer);  // Remove the previously selected state's layer
    }

    if (key !== 'None') {
      var region = arg_states.filter(ee.Filter.eq('NAME_1', key));
      Map.centerObject(region, 6);
      selectedLayer = Map.addLayer(region.style(vis_params_states), {}, key);  // Save the current state layer
    } else {
      selectedLayer = null;  // Reset if "None" is selected
    }
  }
});

// Main Panel
var panel3 = ui.Panel({
  widgets: [dropdown1],
  style: {width: '400px', backgroundColor: '#82E0AA', position: "bottom-left"},
  layout: ui.Panel.Layout.flow('vertical'),
});

//---------------------------------------------------------
// Add drop-down menu for farm types
//---------------------------------------------------------

// Get distinct farm levels from the 'BS' column
var uniqueFarms = farms.distinct(['BS']);

// Collect the distinct values into a list
var farmLevelsList = uniqueFarms.aggregate_array('BS').distinct();

// Print the list of unique farm levels to confirm
print('Unique Farm Levels:', farmLevelsList);

var selectedLayer = null;  // Variable to keep track of the selected layer

// Get the unique farm levels and sort them alphabetically
farmLevelsList.evaluate(function(farmList) {
  farmList = farmList.sort();  // Sort the list alphabetically
  farmList.unshift('None');  // Add "None" at the top of the list
  dropdown2.items().reset(farmList);  // Reset dropdown items with the sorted list
  dropdown2.setPlaceholder('Farm Type');
});

// Visualization parameters for the points
var vis_params_farms = {
  'color': 'black',
  'pointSize': 4,
  'fillColor': 'white',
};


// Define the dropdown
var dropdown2 = ui.Select({
  placeholder: 'Select Farm Type',  // Placeholder text
  onChange: function(key){
    if (selectedLayer !== null) {
      Map.remove(selectedLayer);  // Remove the previously selected layer
    }

    if (key !== 'None') {
      var region = farms.filter(ee.Filter.eq('BS', key));
      Map.centerObject(region, 6);
      selectedLayer = Map.addLayer(region.style(vis_params_farms), {}, key);  // Add the selected layer to the map
    } else {
      selectedLayer = null;  // Reset if "None" is selected
    }
  }
});

// Main panel for the dropdown
var panel4 = ui.Panel({
  widgets: [dropdown2],
  style: {width: '400px', backgroundColor: '#82E0AA', position: "bottom-left"},
  layout: ui.Panel.Layout.flow('vertical'),
});


// Add the checkbox and dropdown menu to the control panel
panel1.add(toggleFarmsCheckbox)
.add(panel3)
.add(panel4);

//----------------------------------------------------------
// Create a panel for the legend
//----------------------------------------------------------

var legendPanel = ui.Panel({
  style: {
    position: 'bottom-right',
    padding: '8px 15px'
  }
});


// Define your palette for valid risk levels (1 to 3)
var palette1 = ['#1abc9c', '#f1c40f', '#fd1a25'];

// First, create a mask for -9999 values and paint them grey
var maskInvalid = model.eq(-9999);
Map.addLayer(maskInvalid.selfMask(), {palette: ['#ccd1d1']}, 'Absent risk');

// Now apply the palette to valid risk levels (1 to 3)
var validRiskLevels = model.updateMask(model.neq(-9999));  // Mask out -9999 values
Map.addLayer(validRiskLevels, {
  min: 1, 
  max: 3, 
  palette: palette1
}, 'Risk model');

// Legend title
var legendTitle = ui.Label({
  value: 'Interaction risk',
  style: {fontWeight: 'bold', fontSize: '14px', margin: '0 0 4px 0'}
});
legendPanel.add(legendTitle);

// Function to create a row in the legend
function makeLegendRow(color, name) {
  var colorBox = ui.Label({
    style: {
      backgroundColor: color,
      padding: '10px',
      margin: '4px'
    }
  });
  var description = ui.Label({
    value: name,
    style: {margin: '4px'}
  });
  return ui.Panel({
    widgets: [colorBox, description],
    layout: ui.Panel.Layout.Flow('horizontal')
  });
}

// Add color boxes and labels for each value (1, 2, 3)
legendPanel.add(makeLegendRow('#ccd1d1', 'Absent'));
legendPanel.add(makeLegendRow('#1abc9c', 'Low'));
legendPanel.add(makeLegendRow('#f1c40f', 'Medium'));
legendPanel.add(makeLegendRow('#fd1a25', 'High'));
 
// Add the legend panel to the map
Map.add(legendPanel);

// Display the Argentina borders
Map.addLayer(arg_states.style({color: 'black', fillColor: '#00000000', width: 1}), null, 'Argentina', true);
Map.addLayer(farms.style({color:'black', width: 1, fillColor:'white', pointSize:4}), null, 'Farm locations', false);


// -------------------------------------------------------------
// Toggle farms layer visibility using a checkbox widget
// -------------------------------------------------------------

// Assuming 'farmData' is a FeatureCollection containing farm points
var styledFarms = farms.style({
  color: 'black',      // Black border color
  fillColor: 'white',  // Solid white fill
  width: 1,            // Border width
  pointSize: 5         // Adjust the size of the points
});

// Add the styled farms layer
var farmsLayer = ui.Map.Layer(styledFarms, {}, 'Farms');


// Center the map (optional)
Map.centerObject(model, 5);


//--------------------------------------------------------------------
// Plots
//--------------------------------------------------------------------

// Global variables
var primeraVuelta = true;
var selectedPoints = [];

// Styles
var HIGHLIGHT_STYLE = {color: '8856a7', fillColor: '8856a7C0'};

// Function to get the selected state based on clicked points
function getSelectedState() {
  return arg_states.filterBounds(ee.Geometry.MultiPoint(selectedPoints));
}

// Update the selected state overlay on the map
function updateOverlay() {
  var overlay = getSelectedState().style(HIGHLIGHT_STYLE);
  Map.layers().set(3, ui.Map.Layer({eeObject: overlay, name: "Selected unit(s)"}));
}

// Clears the set of selected points and resets the results panel
function clearResults() {
  selectedPoints = [];
  Map.layers().remove(Map.layers().get(3));
  resultsPanel.clear(); // Clear previous results
  resultsPanel.add(resetButton); // Add the reset button back
}

// Register a click handler for the map
function handleMapClick(location) {
  selectedPoints.push([location.lon, location.lat]); // Add clicked point
  updateOverlay(); // Update the overlay for selected polygons
  updateChart(); // Update the chart based on the selected area
}

// Function to create the reset button
function createResetButton() {
  var button = ui.Button({
    label: 'Reset Selection',
    style: {width: '340px'}
  });
  
  button.onClick(function() {
    clearResults(); // Call clearResults to reset everything
    resultsPanel.add(ui.Label('Click on a state to see results.')); // Add instructions back
  });

  return button;
}

// Function to calculate and display bar chart for the selected state
function makeResultsBarChart(ecoregions) {
  var areasLow = ee.Image.pixelArea().divide(1e6).updateMask(model.eq(1));
  var areasMedium = ee.Image.pixelArea().divide(1e6).updateMask(model.eq(2));
  var areasHigh = ee.Image.pixelArea().divide(1e6).updateMask(model.eq(3));
  
  // Reduce to calculate area for low, medium, and high risk
  var riskAreas = ee.Image.cat([areasLow.rename('Low'), areasMedium.rename('Medium'), areasHigh.rename('High')])
    .reduceRegions({
      collection: ecoregions,
      reducer: ee.Reducer.sum(),
      scale: model.projection().nominalScale(),
      tileScale: 16
    });

// Create the chart from reduced regions

// Create a floating panel to hold the chart
var floatingPanel = ui.Panel({
  layout: ui.Panel.Layout.flow('horizontal'),
  style: {
    position: 'bottom-left',
    width: '100%', // Set the panel width here
    height: '400px', // You can adjust height as needed
    padding: '8px'
  }
});

var chart = ui.Chart.feature.byFeature(riskAreas, 'NAME_1', ['Low', 'Medium', 'High']) 
    .setChartType('ColumnChart')
    .setOptions({
      title: 'Area by Risk Category',
      titleTextStyle: {
        fontSize: 14,
        bold: true,     // Make the title bold
        italic: true    // Set the title to italic
      },
      vAxis: {
        title: 'Area (sq km)', 
        minValue: 0,
        titleTextStyle: {
          fontSize: 12,  // Set font size for the vertical axis label
          bold: true,     // Make the vertical axis label bold
          italic: false   // Set vertical axis label to non-italic
        },
        textStyle: {
          fontSize: 10,  // Set font size for the vertical axis tick labels
          italic: false   // Ensure tick labels are not italic
        }
      },
      hAxis: {
        title: 'State',
        titleTextStyle: {
          fontSize: 12,  // Set font size for the horizontal axis label
          bold: true,    // Make the horizontal axis label bold
          italic: false,
          fontName: 'Arial' 
        },
        textStyle: {
          fontSize: 10,  // Set font size for the horizontal axis tick labels
          italic: false   // Ensure tick labels are not italic
        }
      },
      series: {
        0: {color: '#abebc6'},    // Low risk
        1: {color: '#f4d03f'},    // Medium risk
        2: {color: '#fd1a25'}     // High risk
      },
      width: 1500,  // Adjust width as needed
      height: 300   // Adjust height if necessary
    });

return chart;
}

// Update the chart using the currently-selected charting function
function updateChart() {
  var selectedState = getSelectedState();
  if (selectedState.size().getInfo() > 0) {
    var chart = makeResultsBarChart(selectedState); // Create the bar chart
    resultsPanel.clear().add(chart); // Clear previous results and display the new chart
    resultsPanel.add(resetButton); // Ensure the reset button is present
  } else {
    resultsPanel.clear().add(ui.Label('No state selected. Click on a state.'));
    resultsPanel.add(resetButton); // Ensure the reset button is present
  }
}

// Initial results panel
var resultsPanel = ui.Panel({style: {position: 'bottom-left'}});
var resetButton = createResetButton(); // Create the reset button

// Add the results panel and button to the map
Map.add(resultsPanel);

// Register the map click handler
Map.onClick(handleMapClick);

// Ensure the results are cleared on the first round
if (!primeraVuelta) {
  clearResults();
} else {
  primeraVuelta = false;
}


//----------------------------------------------------
// Area calculation based on valid risk level pixels
//----------------------------------------------------

// Calculate pixel area in sq km
var pixelArea = ee.Image.pixelArea().divide(1000000);  // Convert sqm to sq km

// Define the list of risk levels present in your model, including "Absent" (i.e., -9999)
var riskLevels = [-9999, 1, 2, 3];  

// Create an empty FeatureCollection to store the results for each province and risk level
var results = ee.FeatureCollection([]);

// Step 1: Initialize results collection
var totalAreaPerProvince = ee.FeatureCollection([]);  // This will store the total area of valid pixels for each province

// Step 2: Iterate over provinces
arg_states.aggregate_array('NAME_1').getInfo().forEach(function(provinceName) {
  
  // Step 3: Mask the raster for valid risk levels (including "Absent")
  var validRiskMask = model.gte(-9999);
  
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
    
    // Add the area calculations for the current risk level and province to the results
    results = results.merge(areaPerRiskLevel);
  });
});

// Step 7: Calculate the percentage of risk areas (including "Absent") relative to the total valid area
results = results.map(function(feature) {
  var provinceName = feature.get('NAME_1');
  var totalArea = totalAreaPerProvince.filter(ee.Filter.eq('NAME_1', provinceName)).first();
  var totalValidAreaKm = ee.Number(totalArea.get('Total_Valid_Area_km'));
  
  var riskAreaKm = ee.Number(feature.get('Risk_Area_km'));
  var percentage = riskAreaKm.divide(totalValidAreaKm).multiply(100);
  
  return feature.set('Total_Valid_Area_km', totalValidAreaKm)
                .set('Percentage', percentage);
});

// Step 8: Export the results as a CSV file to Google Drive
Export.table.toDrive({
  collection: results,
  description: 'RiskLevel_Areas_by_Province_with_Valid_Total',
  fileFormat: 'CSV',
  selectors: ['NAME_1', 'RiskLevel', 'Risk_Area_km', 'Total_Valid_Area_km', 'Percentage']  // Choose columns to include
});
```

