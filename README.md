# 50-Shades-of-Green
For more information on how to used androidrunner please visit the offical github page.
https://github.com/S2-group/android-runner

## Subject Selection
Within the folder "subject-selection" you will find the following two scripts:

- website-downloader: This script is used to download the websites locally. However, each website needs to be manually checked for errors
- trancoList: The following script returns the urls of the top 1000 websites within the tranco list

## Experiment 
### Changes made to the android runner
- The profilers will no longer be running simultaniously, instead each profiler will have its own seperate run
- BatteryStats and PerfumeJS use different website directories in order to ensure that the run stop condition does not interfer with the BatteryStats profiler


### Experiment Configuration

The "exp_config.json" configuration file used to run the android runner experiments is found within the android-runner folder.


### MonkeyRunner
The following experiment was performed using a Nexus 9. Therefore, the monkey runner scripts might not perform as expected on other android devices. All scripts are located within the "android-runner/monkeyrunner" folder. 

### How to host the websites for the experiment?
You can find all the websites used for this experiment in the "webapps" folder. In this folder, there are multiple versions of the websites. 

Within the PerfumeJS folder, all of the websites are the same as the BatteryStats version. However, one major difference is that these websites contain additional JS code, which is needed to stop the experiment prematurely. You can find more information about the run stop conditions here:
https://github.com/S2-group/android-runner/blob/master/docs/run_stop_condition_http_post_tips.md

In addition to the website source code, the websites will have to be hosted locally. We used Apache HTTP Server. 

### HubCtrl 
During the experiment execution we experienced issues using the "usb_handler" provided with the androidrunner. Therefore, we have opted to use a HubCtrl to control the USB power on a port by port basis on some USB hubs.

More information can be found here:
https://github.com/codazoda/hub-ctrl.c

## Data Analysis 

All data gathered from the exerpiments can be found in the "data-analysis" folder. The data is split into two batches with each batch containing five runs.
Additionally, you can find all R scripts used for the data analysis.
