zip:
	git archive --format zip --output ./Haskell.zip master

upload:
	client.py bot -b ./Haskell.zip

all: zip upload
	echo "Done!"
