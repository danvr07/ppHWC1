set datafile separator ","
set xlabel "YearBuilt"
set ylabel "GrLivArea"
set zlabel "SalePrice" offset -5,0,0
splot 'datasets/houseds.csv' using "YearBuilt":"GrLivArea":"SalePrice" with points, -0.44807129465863893 + 9.850924486484171 * x + 106.89764372956043 * y