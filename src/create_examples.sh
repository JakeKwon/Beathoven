LLI="lli"
BEAT="./beathoven"
helperPrint=1

Check(){
basename=`echo $1 | sed 's/.*\\///
                             s/.bt//'`
  printf $BEAT
  printf $1
  printf $LLI

  eval $BEAT < $1 > "$basename.ll"
  eval $LLI "$basename.ll"
  cp "../bet_midi_library/twinkle.mid" "../example_outputs/"$basename.mid


}

INPUTS="../example/*.bt"
for file in $INPUTS
do
  # echo "$file"
  Check $file
done