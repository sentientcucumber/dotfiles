import re
from subprocess import check_output, CalledProcessError

try:
    output = check_output(['acpi', '-b'], universal_newlines=True)
except CalledProcessError:
    print('Error running "acpi -b"')

# Sample "acpi -b" output:
#   Battery 0: Discharging, 86%, 02:10:07 remaining
#   Battery 1: Full, 100%
# Capture the state of the battery (Charging, Discharging, Full or Unknown), the
# percentage of charge, and time remaining.
regex = re.compile('^Battery \d+: (\w+), (\d+)%(?:, ([\d:]+))?.*$', re.M)
batteries = regex.findall(output)
sum = 0

for battery in batteries:
    state = battery[0]

    # The smaller of my two laptop batteries is considered depleated at 5%, and
    # the state becomes "Unknown". For more accurate readings, I don't want that
    # 5% to be part of the calculations.
    if state != 'Unknown':
        sum += int(battery[1])

count = len(batteries)
percentage = int(sum / (count * 100) * 100)

# Display the battery percentage
print('{}%'.format(percentage))

# TODO Add battery icon to act as the label. Change the icon based on the amount
# of battery left. If plugged in (at least one state is Charging), then display
# the plug.
# TODO Change some sort of color based on the amount of battery left. I'm not
# sure where this would be used (possibly on underline, highlighting the percent
# left, not really sure). Decide where it would go before spending time on
# implementation.
# TODO Calculate the amount of time remaining. Pretty low-priority as I rarely
# use it, but could be useful to others.
