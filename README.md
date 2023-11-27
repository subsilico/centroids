# Centroids -- A putative log file analysis pipleline for anomaly detection

Centroids for anomaly detection answers the question of how do I know if I have seen something similar in my logs before?

Centroids3.Rmd updates the math to use Hotelling's T^2 statistic to compare samples:

![Hotelling's](https://raw.githubusercontent.com/subsilico/centroids/master/p-v-dist-centroids.png)

This graph shows how cluster distance is related to Hotelling's statistic, making cluster analysis unnecessary when predicting anomalies.

For completeness, this is what our example samples look like when visualized and ready for cluster analysis:

![Centroids of behavior](https://raw.githubusercontent.com/subsilico/centroids/master/centroids.png)

You can see there are three clusters of behavior in the sample data.

## Converting logging data into clusters using text embeddings

![Text embedings show lines of logging can create clustered data from which to use for anomaly detection](https://raw.githubusercontent.com/subsilico/centroids/master/logging-to-clustered-samples-using-embeddings.png)

See the embeddings.r file for the anaysis.
