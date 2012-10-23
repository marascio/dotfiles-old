#!/usr/bin/gawk -f

{
    sum   += $1
    sumsq += $1 * $1
    if($1 > max)
        max = $1
    if($1 < min)
        min = $1
}

END {
    mean   = sum / NR
    stddev = sqrt(sumsq / NR - mean**2)
    print "min   :", min
    print "max   :", max
    print "mean  :", mean
    print "stddev:", stddev
}
