for ED2IN in /usr2/postdoc/rykelly/edinputs/ed2in/ESMv01/*
do
  if [ -r $ED2IN ]
  then
    qrun.sh $ED2IN
  fi
done



