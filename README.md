This is the Final Project for CS451 Data Mining at The University of San Francisco. This project hopes to predict the amount of bikes rented in a single day through the San Francisco Bike Share program. The success of the Bay Area Bike Share Program will aid in San Francisco’s goal in reducing CO2 emissions. Supervised learning methods such as Linear Regression, Cross Validation, and Tree ensemble methods are used to create statistical models. Mean Squared Error is the metric used to assess these models. 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Files included:

DataPreprocessing.R
This file imports all the necessary data needed. When reading in the csv files change the directory to match the location of your own files. 
Be sure to sort trip_data files upon importing them.
Data is cleaned and trip_data files are utilized to record bike share activity on a daily basis. 
Daily weather, Giants home-game dates, and SF bicycle day dates are merged into the dataset.
The SF_daily_bikeshare dataset is created using the above mentioned features. This will be the main dataset to draw models from.  


DataExploration.R
This file provides insight on the content of the SF_daily_bikeshare dataset.
On line 2, provide the path for your SF_daily_bikeshare workspace data.
Any analysis of distances or speed will not work if using all three years of data.
This was done when analyzing only on year’s worth of data. The addition, loss and change of stations make it difficult to analyze distance or speed of trips.

ModelTesting.R
This file contains three rounds worth of modeling. If using full three years of data, only Round 3 of modeling will work as the previous rounds were used with different data.
Feature selection script is included but not utilized in the testing of models as they gave a higher MSE than if using all variables.
Results are plotted to assess the MSE of different models. 

SF_daily_bikeshare_dataset.RData
Final dataset used for produced by the data preprocessing file to use for data exploration and modeling.

Gas_Prices.csv
The average price of gas in San Francisco at the end of each week.

SF_Events.csv
Event column values:
(1) SF Sunday Streets festival. Community is encouraged to bike to such events
(2) Annual Bike to Work Day.

SF_Giants_Home_Games.csv
Dates when the San Francisco Giants baseball team has a home game. SF Muni recommends attendees use public transportation such as Bike Share to get to the games.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Datasets used:

Bay Area Bike Share Data is downloaded from this website:
http://www.bayareabikeshare.com

San Francisco Giants Data can be found here:
http://sanfrancisco.giants.mlb.com/schedule/sortable.jsp?c_id=sf

San Francisco Sunday Streets Data found here:
http://www.sundaystreetssf.com

San Francisco Gas Pricing Data found here:
https://www.eia.gov/dnav/pet/hist/LeafHandler.ashx?n=PET&s=EMM_EPM0_PTE_Y05SF_DPG&f=W

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Packages used: tree, boot, randomForest, leaps, (optional: geosphere, measurements)

Authors: Carlos Cortez-Aguilera, Lance Fernando
