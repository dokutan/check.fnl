# Tests

Tests work by comparing the result of running check.fnl against a known good result for each test case. Test cases are Fennel files in the ``in`` directory, the expected results are text files in the ``out`` directory.

## Run tests

From the ``test`` directory:
```
./test.fnl
```
or
```
fennel test.fnl
```

## Add tests

1. Add Fennel files to the ``in`` directory
2. From the ``test`` directory run ``./prepare.fnl`` or ``fennel prepare.fnl``
3. Check the newly created files in the ``out`` directory

## Update tests

1. Edit Fennel files in the ``in`` directory
2. Delete the corresponding files from the ``out`` directory
3. From the ``test`` directory run ``./prepare.fnl`` or ``fennel prepare.fnl``
4. Check the newly created files in the ``out`` directory
