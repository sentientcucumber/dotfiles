import re
from subprocess import check_output, CalledProcessError

try:
    output = check_output(['acpi', '-b'], universal_newlines=True)
except CalledProcessError:
    print('Error running "acpi -b"')

regex = re.compile('^Battery \d+: (?P<state>\w+), (?P<charge>\d+).*$', re.M)
batteries = regex.findall(output)
count = len(batteries)
sum = 0

for battery in batteries:
    state = battery[0]

    # The smaller of my two laptop batteries is considered depleated at 5%, and
    # the state becomes "Unknown". For more accurate readings, I don't want that
    # 5% to be part of the calculations.
    if state != 'Unknown':
        sum += int(battery[1])

# Display the battery percentage
print(sum / (count * 100) * 100, '%')
