# zip-fuse-fs

FUSE filesystem to allow browsing Zip files without extracting them

# Intro

I'm mainly writing this to learn more Haskell, but it's slightly useful.

# Usage

- Compile using Haskell Stack: `stack build`
- Mount wherever you want, e.g.: `stack exec -- zip-fuse-fs /unzip`
- Optionally, run in foreground: `stack exec -- zip-fuse-fs /unzip -d`

The root filesystem (`/`) is exposed at the specified mount point.  Any file
ending in `.zip` will also show up as a directory ending in `.zip.d` containing
the Zip file's contents as readable files.

## Example session

Zip file in home dir:

```
/home/bhaskell$ unzip -l test.zip
Archive:  test.zip
  Length      Date    Time    Name
---------  ---------- -----   ----
     1140  2017-09-18 01:31   README.md
    13619  2017-09-22 15:33   src/Main.hs
      482  2017-09-22 14:23   .stack-work/logs/HFuse-0.2.4.5.log
      978  2017-09-18 01:49   .stack-work/logs/zip-archive-0.3.0.5.log
---------                     -------
    16219                     4 files
```

Mount the FUSE filesystem:

```
/home/bhaskell$ stack exec -- zip-fuse-fs /mnt/unzip
# ... no output ...
```

Show that it presents a directory for the Zip file:

```
/home/bhaskell$ ls -nld /mnt/unzip/home/bhaskell/test.zip*
-rw------- 1 1000 1000 5528 Sep 22 15:44 /mnt/unzip/home/bhaskell/test.zip
dr-xr-xr-x 2 1000 1000 5528 Sep 22 15:44 /mnt/unzip/home/bhaskell/test.zip.d
```

Find the contents recursively, in `unzip` format to highlight some diffs:

```
/home/bhaskell$ fmt='%9s  %TY-%Tm-%Td %TH:%TM  /│%P\n'
/home/bhaskell$ find /mnt/unzip/home/bhaskell/test.zip.d -printf "$fmt"
¹    5528  2017-09-22 15:44  /│
     1140  2017-09-17 21:31  /│README.md
²    5528  2017-09-22 15:44  /│src
    13619  2017-09-22 11:33  /│src/Main.hs
²    5528  2017-09-22 15:44  /│.stack-work
²    5528  2017-09-22 15:44  /│.stack-work/logs
      482  2017-09-22 10:23  /│.stack-work/logs/HFuse-0.2.4.5.log
      978  2017-09-17 21:49  /│.stack-work/logs/zip-archive-0.3.0.5.log
```

¹: Root directory "size" is currently the same as the overall Zip file
²: Intermediate directories are synthesized, even if not present in the Zip

# Warnings

Don't mount the directory at a subdirectory of the filesystem root.

# Features Roadmap

- [x] Allows browsing Zip files
- [ ] Don't read the entire Zip file every time
- [ ] Don't list the mountpoint itself (why it's a bad idea to mount in `/dir`)
- [ ] Allow specifying the top-level dir to browse (always `/` currently)

# License

The MIT License (MIT)

Copyright (c) 2017 Benjamin R. Haskell

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
