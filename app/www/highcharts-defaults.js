document.addEventListener('DOMContentLoaded', function() { Highcharts.setOptions({
    lang: {
        numericSymbols: ["K", "M", "B", "T", "P", "E"],
        thousandsSep: ','
    },
    chart: {
        style: {fontSize: '10pt', fontFamily: 'opensans', height: 'auto' },
    },
    // https://personal.sron.nl/~pault/#sec:qualitative
    colors: ['#77AADD', '#EE8866', '#EEDD88', '#FFAABB', '#99DDFF', '#44BB99', '#BBCC33', '#AAAA00', '#DDDDDD'],
    title: {text: '', align: 'left', style: {fontSize: '1.5em'}, marginTop: 0, marginBottom: 100}, 
    subtitle: {align: 'left'}, 
    legend: {
        verticalAlign: 'top', align: 'left', enabled: false, 
        itemStyle: {fontSize: '2em', fontFamily: 'opensans'}, 
        padding: 0, margin: 0, itemMarginBottom: 50, itemMarginTop: 0
    }, // we only show legend when grouping.
    exporting: {enabled: false},
    xAxis: {
        startOnTick: true, endOnTick: true,
        lineWidth: 0, tickWidth: 0, gridLineWidth: 1, 
        labels: {style: {fontFamily: 'opensans', fontSize: '1.2em'}},
        title: {enabled: false, text: 'Values', style: {fontFamily: 'opensans', fontSize: '1.4em'}},
        margin: 5
    },
    yAxis: {
        startOnTick: true, endOnTick: true,
        labels: {enabled: false, style: {fontFamily: 'opensans', fontSize: '1.2em'}}, 
        title: {enabled: false, text: 'Values', style: {fontFamily: 'opensans', fontSize: '1.4em'}},
    },
    tooltip: {style: {fontSize: '1.2em'}, useHTML: true},
    plotOptions: {
        series: {
        //    enableMouseTracking: false,
           animation: false,
        //    marker: {enabled: true},
           dataLabels: {
              enabled: true,
              backgroundColor: 'rgba(255, 255, 255, 0.5)',
              padding: 3,
              verticalAlign: 'middle',
              allowOverlap: true,
              style: {
                textOutline: 'none',
                fontSize: '2.5em',
                fontFamily: 'opensans',
                letterSpacing: '1px',
                color: '#404040'
             }
           }
        },
        // sankey: {tooltip: {style: {fontSize: '10em'}}},
     },
    series: {sankey: {tooltip: {style: {fontSize: '10em'}}}},
    credits: {enabled: false},
    responsive: {rules: [
        // rules for small screens.
        // {condition: {minWidth: 0}, chartOptions: {
        //     chart: {style: {fontSize: '1em'}},
        //     tooltip: {style: {fontSize: '1em'}},
        //     plotOptions: {
        //         series: { dataLabels: {style: {fontSize: '1em'}}}
        //     }
        // }},
        // rules for large screens.
        // {condition: {maxWidth: 800}, chartOptions: {
        //     chart: {style: {marginTop: 10}},
        //     xAxis: {labels: {style: {fontSize: '1.1em'}}, title: {style: {fontSize: '1.1em'}}},
        //     yAxis: {labels: {style: {fontSize: '1.1em'}}, title: {style: {fontSize: '1.1em'}}},
        //     legend: {itemStyle: {fontSize: '1.1em'}, itemMarginBottom: 30, itemMarginTop: 0},
        //     plotOptions: {series: {dataLabels: {style: {fontSize: '1.2em'}}}},
        //     tooltip: {style: {fontSize: '0.45em'}}
        // }}
    ]}
})});