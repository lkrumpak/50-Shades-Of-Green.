# noinspection PyUnusedLocal
import time

def enable_bluetooth():
  deveice.shell('adb shell am start -a android.bluetooth.adapter.action.REQUEST_ENABLE')
  time.sleep(1)
  device.shell('adb shell input keyevent 22')
  device.shell('adb shell input keyevent 22')
  device.shell('adb shell input keyevent 66')

def disable_bluetooth():
  deveice.shell('adb shell am start -a android.bluetooth.adapter.action.REQUEST_DISABLE')
  time.sleep(1)
  device.shell('adb shell input keyevent 22')
  device.shell('adb shell input keyevent 22')
  device.shell('adb shell input keyevent 66')