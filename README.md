# 50-Shades-of-Green
For more information on how to used androidrunner please visit the offical github page.
https://github.com/S2-group/android-runner

## Changes made to the android runner
- The profilers will no longer be running simultaniously, instead each profiler will have its own seperate run
- BatteryStats and PerfumeJS use different website directories in order to ensure that the run stop condtion does not interfer with the BatteryStats profiler


## Experiment Configuration

The "exp_config.json" configuration file used to run the android runner experiments is found within the android-runner folder.


## MonkeyRunner
The following experiment was performed using a Nexus 9. Therefore, the monkey runner scripts might not perform as expected on other android devices. All scripts are located within the "android-runner/monkeyrunner" folder. 

## How to host the websites for the experiment?
You can find all the websites used for this experiment in the "webapps" folder. In this folder, there are multiple versions of the websites. 

Within the PerfumeJS folder, all of the websites are the same as the BatteryStats version. However, one major difference is that these websites contain additional JS code, which is needed to stop the experiment prematurely. You can find more information about the run stop conditions here:
https://github.com/S2-group/android-runner/blob/master/docs/run_stop_condition_http_post_tips.md

In addition to the website source code, the websites will have to be hosted locally. We used Apache HTTP Server. 

## Data Analysis 

All data gathered from the exerpiments can be found in the "data-analysis" folder. The data is split into two batches with each batch containing five runs.
Additionally, you can find all R scripts used for the data analysis.
