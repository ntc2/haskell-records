all: Records.html PolyLens.html LabelPolymorphism.html AvoidingProliferationOfTypes.html \
     AvoidingProliferationOfTypesNoOverlap.html

%.html: %.lhs
	pandoc --standalone --toc -f markdown+lhs -t html+lhs $< > $@

# Convert #-sections to plain text, so that ghc doesn't get confused.
# This is dumb, pandoc should support more than two levels of headers
# in markdown+lhs mode :P
%.safe.lhs: %.lhs
	pandoc -f markdown+lhs -t markdown+lhs $< > $@
