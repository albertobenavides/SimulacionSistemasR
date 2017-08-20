magick convert xc:white xc:red xc:orange xc:yellow xc:green1 xc:cyan xc:blue xc:blueviolet xc:black +append -filter Cubic -resize 600x30! -flop rainbow_lut.png

for /l %%x in (0, 1, 9) do magick convert ../img/g0%%x.png -colorspace gray ../img/rainbow_lut.png -clut ../img/g0%%x.png

for /l %%x in (10, 1, 50) do magick convert ../img/g%%x.png -colorspace gray ../img/rainbow_lut.png -clut ../img/g%%x.png
