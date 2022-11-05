from AndroidRunner.Device import Device
#import handle_bluetooth as bt
import time

def main(device: Device, *args: tuple, **kwargs: dict):
    # Set brightness to lowest level
    device.shell("settings put system screen_brightness 0")
    # Disable location services
    device.shell("settings put secure location_mode 0")
    # Disable bluetooth
    disable_bluetooth(device)


def disable_bluetooth(device: Device):
  device.shell('am start -a android.settings.BLUETOOTH_SETTINGS ')
  time.sleep(1)
  device.shell('input keyevent 19')
  device.shell('input keyevent 23')
