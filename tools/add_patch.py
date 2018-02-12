#!/usr/bin/env python3

import re

TAG_REGEXP = re.compile('^(.*?)(\d*):\s*(.*)')
SECTION_REGEXP = re.compile('^%([^\s]+)')
COMMENT_REGEXP = re.compile('^\s*$|^\s*#.*')


def is_section(line):
    res = SECTION_REGEXP.search(line)
    if res:
        if res.group(1) in ('prep', 'install', 'description', 'package'):
            return res.group(1)
    return False


def add_patch(patch, spec, marker=None):
    lines = spec.split('\n')
    rlines = []
    state = 'source'
    last_patch_id = ''
    last_tag = None
    for line in lines:
        # Lookup the first Source*: tag
        if state == 'source':
            res = TAG_REGEXP.search(line)
            if res and res.group(1).lower() == 'source':
                state = 'patch'
        # Insert the patch before the start of a section or at the end
        # of Patch*: and Source*: tags
        elif state == 'patch':
            res = TAG_REGEXP.search(line)
            if COMMENT_REGEXP.search(line):
                pass
            elif res:
                last_tag = res.group(1).lower()
                if last_tag == 'patch':
                    if res.group(3) == patch:
                        rlines = lines
                        break
                    last_patch_id = res.group(2)
            sect = is_section(line)
            if sect or (last_tag and last_tag not in ('source', 'patch')):
                if last_patch_id == '':
                    patch_id = 0
                else:
                    patch_id = int(last_patch_id) + 1
                rlines.append('Patch%d: %s' % (patch_id, patch))
                if sect == 'prep':
                    state = 'endprep'
                else:
                    state = 'prep'
        # Lookup the %prep section
        elif state == 'prep':
            sect = is_section(line)
            if sect == 'prep':
                state = 'endprep'
        # Insert %patch at the end of the %prep section after the last %patch
        # command or lookup for the optional marker
        elif state == 'endprep':
            if marker and marker == line:
                state = 'marker'
            if not COMMENT_REGEXP.search(line):
                res = SECTION_REGEXP.search(line)
                if res:
                    if not (res.group(1).startswith('patch') or
                            res.group(1) in ('setup', 'if', 'endif')):
                        rlines.append('%%patch%d -p1' % patch_id)
                        state = 'end'
        # Insert %patch before the first empty line or after the last
        # %patch command
        elif state == 'marker':
            if not COMMENT_REGEXP.search(line) or line == '':
                res = SECTION_REGEXP.search(line)
                if res:
                    if not (res.group(1).startswith('patch') or
                            res.group(1) in ('setup', 'if', 'endif')):
                        rlines.append('%%patch%d -p1' % patch_id)
                        state = 'end'
                elif line == '':
                    rlines.append('%%patch%d -p1' % patch_id)
                    state = 'end'
        rlines.append(line)
    return '\n'.join(rlines)


if __name__ == "__main__":
    import sys

    if len(sys.argv) not in (3, 4):
        sys.stderr.write('Usage: %s <patch> <spec file> [<marker>]')
        sys.exit(1)

    with open(sys.argv[2]) as f:
        spec = f.read()
    if len(sys.argv) == 3:
        new_spec = add_patch(sys.argv[1], spec)
    else:
        new_spec = add_patch(sys.argv[1], spec, sys.argv[3])
    with open(sys.argv[2], 'w') as f:
        f.write(new_spec)

# add_patch.py ends here
