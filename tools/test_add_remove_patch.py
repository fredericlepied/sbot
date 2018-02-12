
import unittest

from add_patch import add_patch
from remove_patch import remove_patch


class TestAddPatch(unittest.TestCase):

    def test_add_patch_present(self):
        self.assertEqual(add_patch('my.patch', PATCH_MIN_SPEC),
                         PATCH_MIN_SPEC)

    def test_add_patch_absent(self):
        self.assertEqual(add_patch('my.patch', MIN_SPEC),
                         PATCH_MIN_SPEC)

    def test_add_patch_second(self):
        self.assertEqual(add_patch('second.patch', PATCH_MIN_SPEC),
                         PATCH2_MIN_SPEC)

    def test_add_patch_complex(self):
        self.assertEqual(add_patch('my.patch', COMPLEX_SPEC),
                         PATCH_COMPLEX_SPEC)

    def test_add_patch_marker(self):
        self.assertEqual(add_patch('my.patch', MARKER_SPEC, "# marker"),
                         PATCH_MARKER_SPEC)


class TestRemovePatch(unittest.TestCase):

    def test_remove_patch_present(self):
        self.assertEqual(remove_patch('my.patch', PATCH_MIN_SPEC),
                         MIN_SPEC)

    def test_remove_patch_absent(self):
        self.assertEqual(remove_patch('my.patch', MIN_SPEC),
                         MIN_SPEC)

    def test_remove_patch_second(self):
        self.assertEqual(remove_patch('second.patch', PATCH2_MIN_SPEC),
                         PATCH_MIN_SPEC)

    def test_remove_patch_complex(self):
        self.assertEqual(remove_patch('my.patch', PATCH_COMPLEX_SPEC),
                         COMPLEX_SPEC)

    def test_remove_patch_marker(self):
        self.assertEqual(remove_patch('my.patch', PATCH_MARKER_SPEC),
                         MARKER_SPEC)


MIN_SPEC = '''
%global var 1
Source0: src.tgz
%prep
%setup -q
%install
'''

PATCH_MIN_SPEC = '''
%global var 1
Source0: src.tgz
Patch0: my.patch
%prep
%setup -q
%patch0 -p1
%install
'''

PATCH2_MIN_SPEC = '''
%global var 1
Source0: src.tgz
Patch0: my.patch
Patch1: second.patch
%prep
%setup -q
%patch0 -p1
%patch1 -p1
%install
'''

COMPLEX_SPEC = '''
Source0: src.tgz
Patch100: fix.patch

# comment1
#patch200: old.patch

Summary: summary
%description
%prep
%setup -q

%if 0%{?var} == 6
%patch100 -p1
%endif

%install
'''

PATCH_COMPLEX_SPEC = '''
Source0: src.tgz
Patch100: fix.patch

# comment1
#patch200: old.patch

Patch101: my.patch
Summary: summary
%description
%prep
%setup -q

%if 0%{?var} == 6
%patch100 -p1
%endif

%patch101 -p1
%install
'''

MARKER_SPEC = '''
Source0: src.tgz
%prep
%setup -q

# marker

%install
'''

PATCH_MARKER_SPEC = '''
Source0: src.tgz
Patch0: my.patch
%prep
%setup -q

# marker
%patch0 -p1

%install
'''

if __name__ == "__main__":
    unittest.main()

# test_add_remove_patch.py ends here
