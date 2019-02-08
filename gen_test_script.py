#!/bin/python3

"""
This script will print a sh script to execute every mjtest to test.sh and makes.

For the printed script to run the current directory has to be the root dir of
the project and the compiler has to be compiled in release mode.

To run the tests just execute ./test.sh
"""

import re
import itertools
import os
import stat

def eval_custom_opt_env(fmt):
    return eval_opt_env("Custom:{}".format(fmt))

def eval_opt_env(fmt):
    return ("eval COMPRAKT_OPT={}".format(fmt), "$MJT_INVOCATION || failure\n")

"""
The opts_str will be parsed by the python script below.

Every optimization has to be written in brackets.

If there is more then one variant of an optimization this can be represented by
`[Variant1|Variant2|..]`

If there are optimizations that should be run consecutively this can be expressed by
`[Opt1,Opt2,..]`
"""
opts_str = """[ConstantFolding],[Inline],[ControlFlow],[EarliestPlacement,CommonSubExprElim,CostMinimizingPlacement|CommonSubExprElim]"""

opts = re.compile("\[([\w,|]*)\]")

variants = re.compile("[\w,]+")

opts_res = opts.findall(opts_str)

combinations = []
for i in range(1, len(opts_res)+1):
    combinations.extend(itertools.permutations(opts_res, i))

flattened_combinations = []
iter_tups = combinations
while len(iter_tups) > 0:
    new_tups = []
    for tup in iter_tups:
        for i, elem in enumerate(tup):
            variants_res = variants.findall(elem)
            if len(variants_res) > 1:
                for variant in variants_res:
                    _tup = tup[:i] + (variant,) + tup[i+1:]
                    new_tups.append(_tup)
                    flattened_combinations.append(_tup)
    iter_tups = new_tups

combinations.extend(flattened_combinations)
res_combinations = []
for tup in combinations:
    add = True
    for elem in tup:
        add &= len(variants.findall(elem)) == 1
    if add:
        res_combinations.append(tup)

res_combinations = list(set(res_combinations))
res_combinations.sort(key=len)

script_start = """#!/bin/sh

rm -rf mjtest
git clone https://git.scc.kit.edu/IPDSnelting/mjtest.git
cd mjtest
git submodule update --init --remote

wrapper_code='
#!/bin/bash
set -eux
abspath=$(readlink -f $1)
outabs=$(pwd)/a.out
cd $( dirname "$0" )
pwd
exec ../comprakt/target/$COMPRAKT_CARGO_MODE/compiler-cli --compile -O $COMPRAKT_OPT -o $outabs $abspath
'
echo "$wrapper_code" > ./comprakt_cli_wrapper.sh
chmod +x ./comprakt_cli_wrapper.sh

MJT_INVOCATION="COMPRAKT_CARGO_MODE=release MJ_TIMEOUT=10 MJ_RUN=./comprakt_cli_wrapper.sh ./mjt.py compile --all_exec_tests"

rm -f jenkins_had_failure_during_mjtests
failure() {
  echo "failure with $COMPRAKT_OPT" >> ./jenkins_had_failure_during_mjtests
}

"""

opts_combs = []
opts_combs.append(eval_opt_env("none"))
opts_combs.append(eval_opt_env("aggressive"))
for tup in res_combinations:
    if len(tup) == 1:
        opts_combs.append(eval_custom_opt_env(tup[0]))
    if len(tup) == len(opts_res):
        opts_combs.append(eval_custom_opt_env(",".join([elem for elem in tup])))

for tup in res_combinations:
    if len(tup) != 1 and len(tup) != len(opts_res):
        opts_combs.append(eval_custom_opt_env(",".join([elem for elem in tup])))

max_len = max([len(x[0]) for x in opts_combs])

pad = max_len + (max_len % 4)

opts_combs = [x[0].ljust(pad + 1) + x[1] for x in opts_combs]

script_end = """
if grep "failure" ./jenkins_had_failure_during_mjtests; then
  cat ./jenkins_had_failure_during_mjtests
  exit 1
fi
"""

with open("test.sh", 'w') as script:
    script.write(script_start)
    for comb in opts_combs:
        script.write(comb)
    script.write(script_end)

    st = os.stat(script.fileno())
    os.chmod(script.fileno(), st.st_mode | 0b111)
