(deftemplate dish
    (slot name (type STRING))
    (slot cuisine (type SYMBOL))
    (slot vegetarian (type SYMBOL) (default FALSE))
    (slot sweet (type SYMBOL) (default FALSE))
    (slot sour (type SYMBOL) (default FALSE))
    (slot spiciness (type NUMBER))
    (slot lowcal (type SYMBOL) (default FALSE))
    (slot lowna (type SYMBOL) (default FALSE))
    (slot lowfat (type SYMBOL) (default FALSE))
    (slot highfiber (type SYMBOL) (default FALSE)))

(deftemplate preference
    (slot property (type SYMBOL))
    (slot symbolValue (type SYMBOL))
    (slot numberValue (type NUMBER)))

(deffacts dishes
    (dish (name "Bak kut teh") (cuisine Chinese) (spiciness 2))
    (dish (name "Mee pok") (cuisine Chinese) (sour TRUE) (spiciness 0))
    (dish (name "Char Kway Teow") (cuisine Chinese) (spiciness 1))
    (dish (name "Chicken rice") (cuisine Chinese) (spiciness 0))
    (dish (name "Ban mian") (cuisine Chinese) (spiciness 0) (lowcal TRUE) (lowfat TRUE))
    (dish (name "You char kuey") (cuisine Chinese) (spiciness 0) (lowfat TRUE))
    (dish (name "Kaya toast ") (cuisine Chinese) (sweet TRUE) (spiciness 0) (lowfat TRUE))
    (dish (name "Seafood hor fun") (cuisine Chinese) (spiciness 0) (lowfat TRUE))
    (dish (name "Chai tow Kway") (cuisine Chinese) (spiciness 1))
    (dish (name "Vegetarian bee hoon") (cuisine Chinese) (vegetarian TRUE) (spiciness 0) (lowfat TRUE))
    (dish (name "Kway chap") (cuisine Chinese) (sour TRUE) (spiciness 1) (lowfat TRUE))
    (dish (name "Beef satay") (cuisine Malay) (spiciness 3) (lowcal TRUE) (lowna TRUE))
    (dish (name "Soto ayam") (cuisine Malay) (spiciness 3))
    (dish (name "Lontong") (cuisine Malay) (vegetarian TRUE) (spiciness 0) (lowfat TRUE))
    (dish (name "Acar") (cuisine Malay) (vegetarian TRUE) (sour TRUE) (spiciness 0) (lowcal TRUE) (lowna TRUE) (lowfat TRUE))
    (dish (name "Bakso") (cuisine Malay) (spiciness 0) (lowfat TRUE))
    (dish (name "Otak-otak") (cuisine Malay) (spiciness 2))
    (dish (name "Nasi lemak") (cuisine Malay) (sweet TRUE) (spiciness 2))
    (dish (name "Asam pedas fish") (cuisine Malay) (sour TRUE) (spiciness 3))
    (dish (name "Begedil") (cuisine Malay) (spiciness 0) (lowna TRUE) (lowfat TRUE))
    (dish (name "Kupat tahu") (cuisine Malay) (vegetarian TRUE) (spiciness 0) (lowfat TRUE))
    (dish (name "Tandoori chicken") (cuisine Indian) (spiciness 3))
    (dish (name "Murtabak") (cuisine Indian) (sweet TRUE) (sour TRUE) (spiciness 2))
    (dish (name "Roti prata") (cuisine Indian) (vegetarian TRUE) (spiciness 1) (lowcal TRUE) (lowfat TRUE))
    (dish (name "Appam") (cuisine Indian) (vegetarian TRUE) (sweet TRUE) (spiciness 0) (lowcal TRUE) (lowna TRUE) (lowfat TRUE))
    (dish (name "Masala dosa") (cuisine Indian) (vegetarian TRUE) (spiciness 0) (lowfat TRUE))
    (dish (name "Sup tulang") (cuisine Indian) (spiciness 2))
    (dish (name "Black pepper crab") (cuisine Seafood) (spiciness 2) (lowcal TRUE))
    (dish (name "Fried oyster omelette") (cuisine Seafood) (spiciness 0))
    (dish (name "Sambal stingray") (cuisine Seafood) (spiciness 2) (lowfat TRUE))
    (dish (name "Fish head curry") (cuisine CrossCultural) (sweet TRUE) (sour TRUE) (spiciness 4))
    (dish (name "Laksa") (cuisine CrossCultural) (spiciness 3))
    (dish (name "Ayam buah keluak") (cuisine CrossCultural) (spiciness 2))
    (dish (name "Kueh pie tee") (cuisine CrossCultural) (sweet TRUE) (spiciness 1) (lowcal TRUE) (lowna TRUE) (lowfat TRUE))
    (dish (name "Tauhu goreng") (cuisine CrossCultural) (vegetarian TRUE) (spiciness 2))
    (dish (name "Kueh tutu") (cuisine CrossCultural) (vegetarian TRUE) (sweet TRUE) (spiciness 0) (lowfat TRUE))
    (dish (name "Chicken chop") (cuisine Western) (spiciness 0))
    (dish (name "Pasta") (cuisine Western) (sour TRUE) (spiciness 0))
    (dish (name "Sausage roll") (cuisine Western) (spiciness 2))
    (dish (name "Japanese Ramen") (cuisine Japanese) (spiciness 0))
    (dish (name "Takoyaki") (cuisine Japanese) (spiciness 0))
    (dish (name "Seaweed Roll") (cuisine Japanese) (vegetarian TRUE) (spiciness 0) (lowcal TRUE) (lowna TRUE) (lowfat TRUE)))
