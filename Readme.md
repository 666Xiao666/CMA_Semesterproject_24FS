# Proposal for Semester Project


<!-- 
Please render a pdf version of this Markdown document with the command below (in your bash terminal) and push this file to Github

quarto render Readme.md --to pdf
-->

**Patterns & Trends in Environmental Data / Computational Movement
Analysis Geo 880**

| Semester:      | FS24                                            |
|:---------------|:---------------------------------------- -------|
| **Data:**      | GPS Trajectory data                             |
| **Title:**     | Walking detection from daily GPS trajectories   |
| **Student:**   | Xiao Cui                                        |

## Abstract 
<!-- (50-60 words) -->
Walking is a simple physical activity which can embrace human health and well-being. In this project we detect walking movements from daily GPS trajectories collected by POSMO application. We first summarize common attributes for heustric (rule-based) detection methods. Then we rebuild these approaches and apply them for walking detection. We also compare the performance of rule-based with machine learning methods.

## Research Questions
<!-- (50-60 words) -->
1. What are the characteristics of walking compared with other travel modes?
2. What attributes are used for rule-based travel mode detection in recent studies?
3. How can we rebuild rule-based travel mode detection methods based on R?
4. How do we evaluate the performance of walking detection models?
5. How do different travel mode detection approaches perform?

## Results / products
<!-- What do you expect, anticipate? -->
1. Summary of common attributes in rule-based travel mode detection;
2. Reconstruction of rule-based travel mode detection methods and their applications for walking detection;
3. The performance of different rule-based heuristic detection methods (accuracy, operation time, pros and cons, etc.);
4. (potentially) The performance of machine-learning detection methods (accuracy, operation time, pros and cons, etc.).

## Data
<!-- What data will you use? Will you require additional context data? Where do you get this data from? Do you already have all the data? -->
1. Raw GPS data collected by POSMO Application from the data pool (need more data from others);
2. Footway segments from Stadt Zurich (already have);
3. Terrain from Swisstopo or Google Earth Engine (don't have now, but know how to access);
4. Points of interest (POI) from Open Street Map (OSM) (don't have now, but know how to access);

## Analytical concepts
<!-- Which analytical concepts will you use? What conceptual movement spaces and respective modelling approaches of trajectories will you be using? What additional spatial analysis methods will you be using? -->
Attributes in rule-based mode detection: speed, distance, temporal duration, length of trips, acceleration;
Spatial-temporal trajectory mining;
Additional spatial analysis methods: clusters can be used to remove outliers in data cleaning.

## R concepts
<!-- Which R concepts, functions, packages will you mainly use. What additional spatial analysis methods will you be using? -->
R concepts: we first use summary function to get an overview of our raw data. Other packages or functions for explanatory data handling can also be applied here.
R functions: (1) deriving speed: speed is the main attribute for detecting walking; (2) spatial context: we also consider distance lag or location context as a key attribute for distinguishing indoor movement and walking.
R packages: (1) data handling: readr, dplyr, purrr, lubridate; (2) spatial operation: sf, terra, sfnetwork, igraph; (3) visualisation: ggplot2, plotly, tmap, leaflet; (4) machine learning: (not fixed).

## Risk analysis
<!-- What could be the biggest challenges/problems you might face? What is your plan B? -->
The biggest challenges include: (1) data cleaning (first and foremost step) can be time consuming if dataset is large (also for further data analysis and algorithm operation); Plan B here is to narrow the dataset size, and construct a clear workflow of data cleaning; (2) In this project we focus on walking detection, so it can be a challenge to detect "move" and "stop" for walking due to potential little variations in speed and distance; Plan B here is to 

## Questions? 
<!-- Which questions would you like to discuss at the coaching session? -->
1. How to match GPS trajectories (raw coordinates) with road segments/network (one of rule-based approaches)?
2. Terrain (i.e., slope) can influence walking speed/direction/acceleration largely. How to include this factor into rule-based travel mode detection?
3. It can be a huge workload, but can I use machine learning to detect travel modes (random forest, clustering, etc.) for comparison? If so, any recommendations in R packages/materials?

## References
<!-- references about travel mode detection and walking detection from GPS trajectories -->
Huang, H., Cheng, Y., & Weibel, R. (2019). Transport mode detection based on mobile phone network data: A systematic review. Transportation Research. Part C, Emerging Technologies, 101, 297–312. https://doi.org/10.1016/j.trc.2019.02.008

Marra, A. D., Becker, H., Axhausen, K. W., & Corman, F. (2019). Developing a passive GPS tracking system to study long-term travel behavior. Transportation Research. Part C, Emerging Technologies, 104, 348–368. https://doi.org/10.1016/j.trc.2019.05.006

Sadeghian, P., Håkansson, J., & Zhao, X. (2021). Review and evaluation of methods in transport mode detection based on GPS tracking data. Journal of Traffic and Transportation Engineering/Journal of Traffic and Transportation Engineering, 8(4), 467–482. https://doi.org/10.1016/j.jtte.2021.04.004
