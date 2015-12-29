#!/usr/bin/env zsh

files=("misc.ml" "substitution.ml" "context.ml" "variance.ml" "printing.ml" "typer.ml")

OPTIND=1
verbose=0

show_help () {
    echo "usage: ./stat.sh [-vf]"
    echo "  -h : show this help message"
    echo "  -v : verbose"
}


while getopts "vh" opt; do
    case $opt in
        h)
            show_help
            exit 0
            ;;
        v)
            verbose=1
            ;;
    esac
done

warnings=()

aligned_print () {
    n=$(echo $1 | wc -c)
    n=$((30 - n)) 
    if [ $2 -gt 1 ]; then
        plural="s"
    else
        plural=""
    fi
    printf "    $1%${n}s $2 occurence$plural\n" ":"
}

for f in $files; do
    if [ $verbose -eq 1 ]; then
        echo "Fonctions du fichier $f :"
    else
        echo -n "."
    fi
    liste=$(ocamlc -i $f | grep -o "^val [a-Z0-1_]\+" | sed "s/^val //")
    echo $liste | while read val; do
        nb=$(cat *.ml | grep $val | wc -l)
        if [ $nb -eq 1 ]; then
            warnings+=$val
        fi
        if [ $verbose -eq 1 ]; then
            aligned_print $val $nb
        fi
    done
    if [ $verbose -eq 1 ]; then
        echo
    fi
done

echo
echo "Les fonctions ci-dessous ne sont pas utilisées dans le programme :"
for val in $warnings; do
    echo " - $val"
done

