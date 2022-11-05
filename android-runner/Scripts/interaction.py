import os
import time

from AndroidRunner.Device import Device
from AndroidRunner.MonkeyRunner import MonkeyRunner

MR_FODLER = "/home/pi/green_shades/android-runner/monkeyrunner/"

# noinspection PyUnusedLocal
def main(device: Device, path: tuple, *args: tuple, **kwargs: dict):
   print(f"Website visited: {path}")

   # Don't run the Monkey Runner (MR) interaction script when a PerfumeJS version of a web app is run
   # to avoid MR related issues that come up when a MR scirpt is stopped mid-execution, like in the
   # case of running the PerfumeJS version of a web app. Instead, sleep for 6 seconds, to wait for all
   # the necessary metric to get collected before moving on.
   if 'perfume' in path.lower():
      time.sleep(6)
      return
   # Determines which version of a website is currently visited (full, no-ads, no-analytics)
   website_version = path.split('/')[5]
   if 'ad' in website_version.lower():
      mr_version = 'noAD'
   elif 'analytic' in website_version.lower():
      mr_version ='noAnalytics'
   else:
      mr_version = 'full'

   mr_path = os.path.join(MR_FODLER, f"{path.split('/')[4]}-{mr_version}.monkeyrunner")
   MR_webapp = MonkeyRunner(path=mr_path, monkeyrunner_path="/usr/bin/monkeyrunner", monkey_playback_path="/usr/lib/android-sdk/tools/swt/monkeyrunner/scripts/monkey_playback.py")
   # Run the appropriate MR script for the current web application
   mr_result = MR_webapp.execute_script(device, args, kwargs)

