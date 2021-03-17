# Shiny_app
Data Tidying and Reporting: Take-Away - UC3M 2021

This is a Shiny app to explore the dataset of Pima Indian Diabetes.

The app has 5 different pages: general information, feature description, sample inspection, feature inspection and k-means clustering.

I proceed to detail each of their functionalities.

a) General information: it is composed by a static panel of information about the dataset and the motivation of the project.

b) Feature Description: it is composed by a static description of the variables that are part of the dataset, and their respective classification in: quantitative/qualitative and then either continuous/discrete and ordinal/nominal.
There is also a button that when clicking shows or hides a link with more information about the illness, in case the reader feels interested on getting more details about diabetes.

c) Sample Inspection: this is a page with 3 panels (you can chose the one you want to inspect by a click):
ci) Summary: provides you with summary information about the variables: minimum, maximum and quartiles and mean.
cii) Data Structure: provides you with a quick glance at the general character of each variable and how they are considered by R.
ciii) Data table: provides you with the full data table of the sample. Here you can choose, first, how many rows you want to display in the page. You can also search for a specific value in particular.
      You can change the order of sorting of each variable (min to max or max to min) by clicking at the variable header. You can filter the rows by a specific interval of values of a specific (one or more) variable(s) by clicking under the header of the variable and choosing the interval of values desired.

d) Feature Inspection: Here you will be able to inspect two different variables and their relationship.
First, you can choose two variables and check its histograms, ploted by diabetic and non-diabetic. As both of them are plotly plots, you can put the mouse on top of a bin and you will get information on that specific bin.
Finally, you will see the scatterplot (on the right) of both variables selected. You can click on any point of the scatterplot, and information (the value for each coordinate) will pop up under the scatterplot.

e) K-means Clustering: Here you will find the possibility of choosing 3 variables to perform k-means clustering. As the K-means algorithm requires as an input the number of clusters that one wants to find, you have a slidebar to choose the amount you want.
On the right side of the page, you will see a 3d-scatterplot, where you will distinguish the different clusters, one of a different colour.
Under this, you will find different options to display information of the k-means just performed:
a) Data: displays all the rows of the sample dataset, with a new column regarding what cluster each point belongs to.
b) Centroids: will display the coordinates of the centroids for each of the clusters (coordinates x,y,z).
c) Size: details the amount of datapoints contained by each one of the clusters.
d) Within Cluster Sum of Squares: specifies the variation  that exists in each one of the clusters.
e)Between Sum of squares: it gives information on the total between sum of squres.

Finally, in every one of the pages you can click on a button that says "Generate Report". 
When doing so, you will be able to download a pdf report on the main things of the app.

Enjoy!
