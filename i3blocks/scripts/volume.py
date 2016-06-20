import re
from subprocess import check_output, CalledProcessError

try:
    # Sample "amixer sget Master" output:
    #   Simple mixer control 'Master',0
    #   Capabilities: pvolume pvolume-joined pswitch pswitch-joined
    #   Playback channels: Mono
    #   Limits: Playback 0 - 87
    #   Mono: Playback 24 [28%] [-47.25dB] [on]
    output = check_output(['amixer', 'sget', 'Master'], universal_newlines = True)
except CalledProcessError:
    print('Error running "amixer"!')

# The last line has the bit of information we need.
mono_line = output.split('\n')[4]

# Capture the percentage
regexp = re.compile('^  Mono: Playback \d+ \[(\d+)%\].*$')
groups = regexp.findall(mono_line)
percentage = int(groups[0])
icon = ''

if percentage > 50:
    icon = ''
elif percentage > 1:
    icon = ''
else:
    icon = ''

print('{} {:3}%'.format(icon, percentage))

# TODO Threw this together at 11:40 pm. I should probably double check things to
# make sure it's sane.
