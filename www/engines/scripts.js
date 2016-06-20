function makeplot() {
		    Plotly.d3.csv("../dataRepo/sp_500.csv", function(data){ processData(data) } );
		};

function processData(allRows) {
    console.log(allRows);
    var x = [], y = [], standard_deviation = [];

    for (var i=0; i<allRows.length; i++) {
        row = allRows[i];
        x.push( row['date'] );
        y.push( row['close'] );
    }
    console.log( 'X',x, 'Y',y, 'SD',standard_deviation );
    makePlotly( x, y, standard_deviation );
}

function makePlotly( x, y, standard_deviation ){
    var plotDiv = document.getElementById("plot");
    var traces = [{
        x: x, 
        y: y
    }];

    Plotly.newPlot('myDiv', traces, 
        {title: 'Plot of S&P 500 data (1995-2015)'});
};