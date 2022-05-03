#! /bin/bash

for i in 0 1 2 3 4 5 6 7 8 9
do
    echo "Replacing '=item $i' to '=item C<$i>'"
    grep -rl "=item $i" * | xargs sed -i "s/=item $i/=item C<$i>/g"
done

# use these commands below only once!
grep -rl "HUGE" parsec/parsec-2.1/pkgs/apps/ferret | xargs sed -i "s/HUGE/HUGE_VAL/g"
grep -rl "HUGE" parsec/parsec-2.1/pkgs/netapps/netferret | xargs sed -i "s/HUGE/HUGE_VAL/g"
