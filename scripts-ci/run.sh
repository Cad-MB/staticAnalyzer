#!/bin/bash

ANALYZER=./_build/default/src/main.exe
TIMEOUT=2s
HTML=result.html
DETAIL=$1

function escape {
    echo -n "$1" | sed 's/&/\&amp;/g; s/</\&lt;/g; s/>/\&gt;/g; s/"/\&quot;/g; s/'"'"'/\&#39;/g' >> $HTML
}

function escape_multiline {
    echo -n "$1" | sed 's/&/\&amp;/g; s/</\&lt;/g; s/>/\&gt;/g; s/"/\&quot;/g; s/'"'"'/\&#39;/g; s/$/<br>/g; s/ /\&nbsp;/g' >> $HTML
}

function print_header {
    cat > $HTML <<EOF
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
<meta charset="utf-8"/>
<title>Test results</title>
</head>
<style>
body { font-family:sans-serif; margin: auto; width: 85em; }
table.test { margin-left: 2em; }
.test td { vertical-align: top; margin: 0; padding: 0; }
.summary td { background-color: #eee; padding: 1ex; }
.summary2 td { background-color: #eef; padding: 1ex; }
.filename { background-color: #e0e0ff; font-family: monospace; font-weight: bold; padding: 1ex 2ex; margin: 0; }
.code { padding: 1ex 2ex; margin: 0; color: #112; background-color: #eee; font-family: monospace; width: 80em; overflow: auto; }
.result { padding: 1ex 2ex;  margin: 0; background-color: #e0ffe0; font-family: monospace; width: 80em; overflow: auto; }
.resultbad {  padding: 1ex 2ex; margin: 0; color: #500; background-color: #ffe0e0; font-family: monospace; width: 80em; overflow: auto; }
.error { font-weight: bold; color: #f22; }
h2 { margin-top: 1em; border-top: 1px solid gray; padding-top: 1em; }
</style>
<body>
EOF
}

function print_footer {
    echo "</body><html>" >> $HTML
}

function compile {
    eval `opam config env`
    dune build
    if [ ! -x $ANALYZER ]
    then
        echo "<p><div class='error'>Build failed</div>" >> $HTML
        print_footer
        echo
        echo "*** BUILD FAILED ***"
        echo
        exit 1
    fi
}

function run_test_file {
    file=$1
    options=$2
    echo "  Testing `basename $file`"
    if [ "x$DETAIL" == "xdetail" ]
    then
        result="`timeout $TIMEOUT ./$ANALYZER $options -trace -nonreldebug $file 2>&1`"
    else
        result="`timeout $TIMEOUT ./$ANALYZER $options $file 2>&1`"
    fi
    if [ $? -eq 124 ]; then echo "  -> timeout"; result="TIMEOUT"; fi

    if [ "x$DETAIL" == "xdetail" ]
    then
        expected_file="`dirname $1`/`basename $1 .c`.detail.expected"
    else
        expected_file="`dirname $1`/`basename $1 .c`.expected"
    fi

    if [ -f "$expected_file" ]
    then
        expected="`cat $expected_file`"
    elif [ "x$TEACHER_MODE" != "x" ]
    then
        echo "  -> creating missing expected file $expected_file"
        echo "$result" > $expected_file
        expected="$result"
    else
        echo "  -> missing expected file $expected_file"
        expected=""
    fi

    TESTED_TOTAL=$((TESTED_TOTAL+1))
    TESTED_DIR=$((TESTED_DIR+1))

    echo "<table class='test' width='100' >" >> $HTML
    echo -n "<tr><td><a name='test$TESTED_TOTAL'><div class='filename'>" >> $HTML
    escape "$file"
    echo "</div></a></td></tr>" >> $HTML
    echo -n "<tr><td><div class='code'>" >> $HTML
    escape_multiline "`cat $file`"
    echo "</div></td></tr>" >> $HTML
    if [ "x$expected" == "x" ]
    then
        # missing expected
        echo -n "<tr><td><div class='result'>" >> $HTML
        escape_multiline "$result"
        echo "</div></td></tr>" >> $HTML
        echo -n "<tr><td><div class='resultbad'>" >> $HTML
        echo "<b>No expected file found</b><br>" >> $HTML
        echo "</div></td></tr>" >> $HTML
        echo "  -> missing expected"
        MISSING_TOTAL=$((MISSING_TOTAL+1))
        MISSING_DIR=$((MISSING_DIR+1))
        ERRS="$ERRS <li><a href='#test$TESTED_TOTAL'><tt>$file $options</tt></a>"
        HAS_ERR=1
    elif [ "x$expected" == "x$result" ]
    then
        # OK
        echo -n "<tr><td><div class='result'>" >> $HTML
        escape_multiline "$result"
        echo "</div></td></tr>" >> $HTML
        OK_TOTAL=$((OK_TOTAL+1))
        OK_DIR=$((OK_DIR+1))
    else
        # bad
        echo -n "<tr><td><div class='result'>" >> $HTML
        echo "<b>Expected result:</b><br>" >> $HTML
        escape_multiline "$expected"
        echo "</div></td></tr>" >> $HTML
        echo -n "<tr><td><div class='resultbad'>" >> $HTML
        echo "<b>Actual result:</b><br>" >> $HTML
        escape_multiline "$result"
        echo "</div></td></tr>" >> $HTML
        echo "  -> bad"
        BAD_TOTAL=$((BAD_TOTAL+1))
        BAD_DIR=$((BAD_DIR+1))
        ERRS="$ERRS <li><a href='#test$TESTED_TOTAL'><tt>$file $options</tt></a>"
        HAS_ERR=1
    fi
    echo "</table><br>" >> $HTML
}

function run_test_dir {
    dir=$1
    options="$2"
    TESTED_DIR=0
    MISSING_DIR=0
    OK_DIR=0
    BAD_DIR=0
    DIR=$((DIR+1))
    echo "Testing directory $dir, options '$options'"
    echo -n "<h2><a name='dir$DIR'>Testing directory <tt>" >> $HTML
    escape "$dir"
    echo -n "</tt> using option <tt>&quot;" >> $HTML
    escape "$options"
    echo "&quot;</tt></a></h2>" >> $HTML
    suf="1"
    for file in $dir/*.c
    do
        if [ -r "$file" ]
        then
            run_test_file "$file" "$options"
        fi
    done
    if [ $TESTED_DIR -eq 0 ]
    then
        echo "<p>Empty directory" >> $HTML
    else
        echo "<h3>Summary for directory <tt>$dir</tt>, options <tt>'$options'</tt></h3><table class='summary'>" >> $HTML
        echo "<tr><td>OK:</td><td style='text-align: right'>$OK_DIR</td></tr>" >> $HTML
        echo "<tr><td>BAD:</td><td style='text-align: right'>$BAD_DIR</td></tr>" >> $HTML
        echo "<tr><td>Missing expected:</td><td style='text-align: right'>$MISSING_DIR</td></tr>" >> $HTML
        echo "<tr><td>Total:</td><td style='text-align: right'>$TESTED_DIR</td></tr>" >> $HTML
        echo "</table><br>" >> $HTML
    fi
    DIRS="$DIRS <tr><td><a href='#dir$DIR'><tt>$dir</tt></a></td><td><tt>$options</tt></td><td style='text-align: right'>$TESTED_DIR</td><td style='text-align: right'>$OK_DIR</td><td style='text-align: right'>$BAD_DIR</td><td style='text-align: right'>$MISSING_DIR</td></tr>"
    echo
}

function summary {
        echo "<h2 style='color: #295'><a name='dirs'>List of directories:</a></h2><table class='summary'><tr><th>directory</th><th>options</th><th>tested</th><th>OK</th><th>BAD</th><th>missing</th></tr>$DIRS</table><br><br>" >> $HTML
        echo "<h2 style='color: #f28'><a name='errors'>Tests with errors:</a></h2><ul>$ERRS</ul><br><br>" >> $HTML
        echo "<h2 style='color: #24f'><a name='summary'>Summary</a></h2><table class='summary2'>" >> $HTML
        echo "<tr><td>OK:</td><td style='text-align: right'>$OK_TOTAL</td></tr>" >> $HTML
        echo "<tr><td>BAD:</td><td style='text-align: right'>$BAD_TOTAL</td></tr>" >> $HTML
        echo "<tr><td>Missing expected:</td><td style='text-align: right'>$MISSING_TOTAL</td></tr>" >> $HTML
        echo "<tr><td>Total:</td><td style='text-align: right'>$TESTED_TOTAL</td></tr>" >> $HTML
        echo "</table><br><br>" >> $HTML
        echo "OK: $OK_TOTAL"
        echo "BAD: $BAD_TOTAL"
        echo "Missing expected: $MISSING_TOTAL"
        echo "Total: $TESTED_TOTAL"
}

function run_test {
    print_header
    compile
    echo "<h1>Test results</h1>" >> $HTML
    echo "<p><a href='#summary'>Go to summary</a><br>" >> $HTML
    echo "<p><a href='#dirs'>Go to list of directories</a><br>" >> $HTML
    echo "<p><a href='#errors'>Go to list of errors</a><br>" >> $HTML
    TESTED_TOTAL=0
    MISSING_TOTAL=0
    OK_TOTAL=0
    BAD_TOTAL=0
    ERRS=""
    DIR=0
    DIRS=""
    HAS_ERR=0

    run_test_dir "tests/01_concrete" "-concrete"
    run_test_dir "tests/02_concrete_loop" "-concrete"
    run_test_dir "tests/03_concrete_assert" "-concrete"
    run_test_dir "tests/04_constant" "-constant"
    #run_test_dir "tests/10_interval" "-interval"
    #run_test_dir "tests/11_interval_cmp" "-interval"
    #run_test_dir "tests/12_interval_loop" "-interval"
    #run_test_dir "tests/13_interval_loop_delay" "-interval -delay 3"
    #run_test_dir "tests/14_interval_loop_delay_unroll" "-interval -unroll 3 -delay 3"
    #run_test_dir "tests/20_reduced" "-parity-interval"

    # répertoire 30_extension à remplir
    #run_test_dir "tests/30_extension" ""

    summary
    print_footer
    if [ $HAS_ERR -eq 1 ]
    then
        echo
        echo "*** TESTS ENDED WITH ERRORS ***"
        echo
        exit 1
    else
        echo
        echo "*** all tests passed ***"
        echo
    fi
}

run_test
