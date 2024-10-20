clone count xs = concat [ take count $ repeat x | x <- xs ]
