for file in *.sco; do \
   stem=`basename $file .sco` ; \
   echo $stem ; \
   (csound -A -d -o $stem.wav -s $stem.orc $stem.sco | grep error) \
done

   # (csound -A -d -o /tmp/csound.wav -s $stem.orc $stem.sco | grep error) \

   # csound -A -d -o /tmp/csound.wav -s $stem.orc $stem.sco && \
   # play /tmp/csound.wav ; \
