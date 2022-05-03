#!/bin/bash

# assuming you are on your git repo folder and you generated a large file list (bigger than 50MB in this case), e.g.:
find . -size +50M | cat > file-bigger-than-50MB.txt

flist="file-bigger-than-50MB.txt"
while IFS= read -r file
do
  git lfs track "$file"
done < "$flist"

# do not forget to run `git add .gitattributes` afterwards, before you add and commit the large files