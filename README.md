# centroids
Centroids for anomaly detection

Centroids3.Rmd updates the math to use Hotelling's T^2 statistic to compare samples:

![Hotelling's](https://raw.githubusercontent.com/subsilico/centroids/master/p-v-dist-centroids.png)

This graph shows how cluster distance is related to Hotelling's statistic, making cluster analysis unnecessary when predicting anomalies.

For completeness, this is what our example samples look like when visualized and ready for cluster analysis:

![Centroids of behavior](https://raw.githubusercontent.com/subsilico/centroids/master/centroids.png)

You can see there are three clusters of behavior in the sample data.
