magick convert xc:white xc:red xc:orange xc:yellow xc:green1 xc:cyan xc:blue xc:blueviolet xc:black +append -filter Cubic -resize 600x30! -flop rainbow_lut.png

for /l %%x in (0, 1, 9) do magick convert challengeG00%%x.png -colorspace gray rainbow_lut.png -clut result0%%x.png

for /l %%x in (0, 1, 9) do magick convert challengeG01%%x.png -colorspace gray rainbow_lut.png -clut result1%%x.png

for /l %%x in (0, 1, 7) do magick convert challengeG02%%x.png -colorspace gray rainbow_lut.png -clut result2%%x.png
