import re
from subprocess import check_output, CalledProcessError

try:
    # Sample "acpi -b" output:
    #   Battery 0: Discharging, 86%, 02:10:07 remaining
    #   Battery 1: Full, 100%
    battery_output = check_output(['acpi', '-b'], universal_newlines = True)

    # Sample "acpi -a" output:
    #   Adapter 0: on-line
    charger_output = check_output(['acpi', '-a'], universal_newlines = True)
except CalledProcessError:
    print('Error running "acpi"!')

# Capture percentage of charge, state, and time remaining.
battery_re = re.compile('^Battery \d+: (\w+), (\d+)%(?:, ([\d:]+))?.*$', re.M)
batteries = battery_re.findall(battery_output)
sum = 0

for battery in batteries:
    state = battery[0]

    # FIXME I have two batteries in my laptop. The first is considered depleated
    # with 5% remaining. I would like the calculation to eliminate this (minor)
    # error.
    sum += int(battery[1])

# Determine total percentage
count = len(batteries)
percentage = int(sum / (count * 100) * 100)
icon = ''

if percentage > 80:
    icon = '\uf240'             # full battery
elif percentage > 60:
    icon = '\uf241'             # 3/4 battery
elif percentage > 40:
    icon = '\uf242'             # 1/2 battery
elif percentage > 20:
    icon = '\uf243'             # 1/4 battery
else:
    icon = '\uf243'             # empty battery

charger_re = re.compile('^Adapter \d: (?P<status>\w+)-line$')
adapter = charger_re.match(charger_output)
is_charging = adapter.group('status')

if is_charging == 'on':
    icon = '\uf1e6'             # plug
    
# Display the battery percentage
print('{} {}%'.format(icon, percentage))

# TODO Calculate time remaining.
