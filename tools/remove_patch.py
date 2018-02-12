#!/usr/bin/env python3

import re


def remove_patch(patch, spec):
    lines = spec.split('\n')
    rlines = []
    state = 'patch'
    patch_regexp = re.compile('Patch(\d+):\s*%s' % patch)
    for line in lines:
        to_remove = False
        if state == 'patch':
            res = patch_regexp.search(line)
            if res:
                id = res.group(1)
                apply_regexp = re.compile('patch%s\s*' % id)
                to_remove = True
                state = 'apply'
        elif state == 'apply':
            res = apply_regexp.search(line)
            if res:
                to_remove = True
                state = 'end'
        if not to_remove:
            rlines.append(line)
    return '\n'.join(rlines)


if __name__ == "__main__":
    import sys

    if len(sys.argv) != 3:
        sys.stderr.write('Usage: %s <patch> <spec file>')
        sys.exit(1)

    with open(sys.argv[2]) as f:
        spec = f.read()
    new_spec = remove_patch(sys.argv[1], spec)
    with open(sys.argv[2], 'w') as f:
        f.write(new_spec)

# remove_patch.py ends here
