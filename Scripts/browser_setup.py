import os

from AndroidRunner.Device import Device
from AndroidRunner.MonkeyRunner import MonkeyRunner

MR_FODLER = "/home/pi/green_shades/android-runner/monkeyrunner/"
MR_SCRIPTS = {"chrome": "startChrome", "opera": "startOpera"}


def main(device: Device, path: tuple, *args: tuple, **kwargs: dict):
    browser_name = kwargs["browser"].package_name

    if 'chrome' in browser_name.lower():
        mr_path = mr_path = os.path.join(
            MR_FODLER, f"{MR_SCRIPTS['chrome']}.monkeyrunner"
        )
    else:
        mr_path = os.path.join(
            MR_FODLER, f"{MR_SCRIPTS['opera']}.monkeyrunner"
        )
    print('mr_path', mr_path)
    MR_browser = MonkeyRunner(path=mr_path, monkeyrunner_path="/usr/bin/monkeyrunner", monkey_playback_path="/usr/lib/android-sdk/tools/swt/monkeyrunner/scripts/monkey_playback.py")
    mr_browser_result = MR_browser.execute_script(device, args, kwargs)
    print(f"Monkey runner browser setup script result: {mr_browser_result}")