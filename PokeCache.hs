module PokeCache where
import Pokemon
pokedex = [Pokemon {name = "Victini", types = [Psychic,Fire], dex = 494, stats = Stats {hp = 100, atk = 100, def = 100, satk = 100, sdef = 100, spd = 100, total = 600}, generation = 5},Pokemon {name = "Snivy", types = [Grass], dex = 495, stats = Stats {hp = 45, atk = 45, def = 55, satk = 45, sdef = 55, spd = 63, total = 308}, generation = 5},Pokemon {name = "Servine", types = [Grass], dex = 496, stats = Stats {hp = 60, atk = 60, def = 75, satk = 60, sdef = 75, spd = 83, total = 413}, generation = 5},Pokemon {name = "Serperior", types = [Grass], dex = 497, stats = Stats {hp = 75, atk = 75, def = 95, satk = 75, sdef = 95, spd = 113, total = 528}, generation = 5},Pokemon {name = "Tepig", types = [Fire], dex = 498, stats = Stats {hp = 65, atk = 63, def = 45, satk = 63, sdef = 45, spd = 45, total = 308}, generation = 5},Pokemon {name = "Pignite", types = [Fire,Fighting], dex = 499, stats = Stats {hp = 90, atk = 93, def = 55, satk = 93, sdef = 55, spd = 55, total = 418}, generation = 5},Pokemon {name = "Emboar", types = [Fire,Fighting], dex = 500, stats = Stats {hp = 110, atk = 123, def = 65, satk = 123, sdef = 65, spd = 65, total = 528}, generation = 5},Pokemon {name = "Oshawott", types = [Water], dex = 501, stats = Stats {hp = 55, atk = 55, def = 45, satk = 55, sdef = 45, spd = 45, total = 308}, generation = 5},Pokemon {name = "Dewott", types = [Water], dex = 502, stats = Stats {hp = 75, atk = 75, def = 60, satk = 75, sdef = 60, spd = 60, total = 413}, generation = 5},Pokemon {name = "Samurott", types = [Water], dex = 503, stats = Stats {hp = 95, atk = 100, def = 85, satk = 100, sdef = 85, spd = 70, total = 528}, generation = 5},Pokemon {name = "Patrat", types = [Normal], dex = 504, stats = Stats {hp = 45, atk = 55, def = 39, satk = 55, sdef = 39, spd = 42, total = 255}, generation = 5},Pokemon {name = "Watchog", types = [Normal], dex = 505, stats = Stats {hp = 60, atk = 85, def = 69, satk = 85, sdef = 69, spd = 77, total = 420}, generation = 5},Pokemon {name = "Lillipup", types = [Normal], dex = 506, stats = Stats {hp = 45, atk = 60, def = 45, satk = 60, sdef = 45, spd = 55, total = 275}, generation = 5},Pokemon {name = "Herdier", types = [Normal], dex = 507, stats = Stats {hp = 65, atk = 80, def = 65, satk = 80, sdef = 65, spd = 60, total = 370}, generation = 5},Pokemon {name = "Stoutland", types = [Normal], dex = 508, stats = Stats {hp = 85, atk = 100, def = 90, satk = 100, sdef = 90, spd = 80, total = 490}, generation = 5},Pokemon {name = "Purrloin", types = [Dark], dex = 509, stats = Stats {hp = 41, atk = 50, def = 37, satk = 50, sdef = 37, spd = 66, total = 281}, generation = 5},Pokemon {name = "Liepard", types = [Dark], dex = 510, stats = Stats {hp = 64, atk = 88, def = 50, satk = 88, sdef = 50, spd = 106, total = 446}, generation = 5},Pokemon {name = "Pansage", types = [Grass], dex = 511, stats = Stats {hp = 50, atk = 53, def = 48, satk = 53, sdef = 48, spd = 64, total = 316}, generation = 5},Pokemon {name = "Simisage", types = [Grass], dex = 512, stats = Stats {hp = 75, atk = 98, def = 63, satk = 98, sdef = 63, spd = 101, total = 498}, generation = 5},Pokemon {name = "Pansear", types = [Fire], dex = 513, stats = Stats {hp = 50, atk = 53, def = 48, satk = 53, sdef = 48, spd = 64, total = 316}, generation = 5},Pokemon {name = "Simisear", types = [Fire], dex = 514, stats = Stats {hp = 75, atk = 98, def = 63, satk = 98, sdef = 63, spd = 101, total = 498}, generation = 5},Pokemon {name = "Panpour", types = [Water], dex = 515, stats = Stats {hp = 50, atk = 53, def = 48, satk = 53, sdef = 48, spd = 64, total = 316}, generation = 5},Pokemon {name = "Simipour", types = [Water], dex = 516, stats = Stats {hp = 75, atk = 98, def = 63, satk = 98, sdef = 63, spd = 101, total = 498}, generation = 5},Pokemon {name = "Munna", types = [Psychic], dex = 517, stats = Stats {hp = 76, atk = 25, def = 45, satk = 25, sdef = 45, spd = 24, total = 292}, generation = 5},Pokemon {name = "Musharna", types = [Psychic], dex = 518, stats = Stats {hp = 116, atk = 55, def = 85, satk = 55, sdef = 85, spd = 29, total = 487}, generation = 5},Pokemon {name = "Pidove", types = [Normal,Flying], dex = 519, stats = Stats {hp = 50, atk = 55, def = 50, satk = 55, sdef = 50, spd = 43, total = 264}, generation = 5},Pokemon {name = "Tranquill", types = [Normal,Flying], dex = 520, stats = Stats {hp = 62, atk = 77, def = 62, satk = 77, sdef = 62, spd = 65, total = 358}, generation = 5},Pokemon {name = "Unfezant", types = [Normal,Flying], dex = 521, stats = Stats {hp = 80, atk = 105, def = 80, satk = 105, sdef = 80, spd = 93, total = 478}, generation = 5},Pokemon {name = "Blitzle", types = [Electric], dex = 522, stats = Stats {hp = 45, atk = 60, def = 32, satk = 60, sdef = 32, spd = 76, total = 295}, generation = 5},Pokemon {name = "Zebstrika", types = [Electric], dex = 523, stats = Stats {hp = 75, atk = 100, def = 63, satk = 100, sdef = 63, spd = 116, total = 497}, generation = 5},Pokemon {name = "Roggenrola", types = [Rock], dex = 524, stats = Stats {hp = 55, atk = 75, def = 85, satk = 75, sdef = 85, spd = 15, total = 280}, generation = 5},Pokemon {name = "Boldore", types = [Rock], dex = 525, stats = Stats {hp = 70, atk = 105, def = 105, satk = 105, sdef = 105, spd = 20, total = 390}, generation = 5},Pokemon {name = "Gigalith", types = [Rock], dex = 526, stats = Stats {hp = 85, atk = 135, def = 130, satk = 135, sdef = 130, spd = 25, total = 505}, generation = 5},Pokemon {name = "Woobat", types = [Psychic,Flying], dex = 527, stats = Stats {hp = 55, atk = 45, def = 43, satk = 45, sdef = 43, spd = 72, total = 313}, generation = 5},Pokemon {name = "Swoobat", types = [Psychic,Flying], dex = 528, stats = Stats {hp = 67, atk = 57, def = 55, satk = 57, sdef = 55, spd = 114, total = 425}, generation = 5},Pokemon {name = "Drilbur", types = [Ground], dex = 529, stats = Stats {hp = 60, atk = 85, def = 39, satk = 85, sdef = 39, spd = 68, total = 327}, generation = 5},Pokemon {name = "Excadrill", types = [Ground,Steel], dex = 530, stats = Stats {hp = 110, atk = 135, def = 60, satk = 135, sdef = 60, spd = 88, total = 508}, generation = 5},Pokemon {name = "Audino", types = [Normal], dex = 531, stats = Stats {hp = 103, atk = 60, def = 86, satk = 60, sdef = 86, spd = 50, total = 445}, generation = 5},Pokemon {name = "Timburr", types = [Fighting], dex = 532, stats = Stats {hp = 75, atk = 80, def = 55, satk = 80, sdef = 55, spd = 35, total = 305}, generation = 5},Pokemon {name = "Gurdurr", types = [Fighting], dex = 533, stats = Stats {hp = 85, atk = 105, def = 85, satk = 105, sdef = 85, spd = 40, total = 405}, generation = 5},Pokemon {name = "Conkeldurr", types = [Fighting], dex = 534, stats = Stats {hp = 105, atk = 140, def = 95, satk = 140, sdef = 95, spd = 45, total = 505}, generation = 5},Pokemon {name = "Tympole", types = [Water], dex = 535, stats = Stats {hp = 50, atk = 50, def = 40, satk = 50, sdef = 40, spd = 64, total = 294}, generation = 5},Pokemon {name = "Palpitoad", types = [Water,Ground], dex = 536, stats = Stats {hp = 150, atk = 65, def = 55, satk = 65, sdef = 55, spd = 69, total = 384}, generation = 5},Pokemon {name = "Seismitoad", types = [Water,Ground], dex = 537, stats = Stats {hp = 105, atk = 85, def = 75, satk = 85, sdef = 75, spd = 74, total = 499}, generation = 5},Pokemon {name = "Throh", types = [Fighting], dex = 538, stats = Stats {hp = 120, atk = 100, def = 85, satk = 100, sdef = 85, spd = 45, total = 465}, generation = 5},Pokemon {name = "Sawk", types = [Fighting], dex = 539, stats = Stats {hp = 75, atk = 125, def = 75, satk = 125, sdef = 75, spd = 85, total = 465}, generation = 5},Pokemon {name = "Sewaddle", types = [Bug,Grass], dex = 540, stats = Stats {hp = 45, atk = 53, def = 70, satk = 53, sdef = 70, spd = 42, total = 310}, generation = 5},Pokemon {name = "Swadloon", types = [Bug], dex = 541, stats = Stats {hp = 55, atk = 63, def = 90, satk = 63, sdef = 90, spd = 42, total = 380}, generation = 5},Pokemon {name = "Leavanny", types = [Bug,Grass], dex = 542, stats = Stats {hp = 75, atk = 103, def = 80, satk = 103, sdef = 80, spd = 92, total = 490}, generation = 5},Pokemon {name = "Venipede", types = [Bug,Poison], dex = 543, stats = Stats {hp = 30, atk = 45, def = 59, satk = 45, sdef = 59, spd = 57, total = 260}, generation = 5},Pokemon {name = "Whirlipede", types = [Bug,Poison], dex = 544, stats = Stats {hp = 40, atk = 55, def = 99, satk = 55, sdef = 99, spd = 47, total = 360}, generation = 5},Pokemon {name = "Scolipede", types = [Bug,Poison], dex = 545, stats = Stats {hp = 60, atk = 90, def = 89, satk = 90, sdef = 89, spd = 112, total = 475}, generation = 5},Pokemon {name = "Cottonee", types = [Grass], dex = 546, stats = Stats {hp = 40, atk = 27, def = 60, satk = 27, sdef = 60, spd = 66, total = 280}, generation = 5},Pokemon {name = "Whimsicott", types = [Grass], dex = 547, stats = Stats {hp = 60, atk = 67, def = 85, satk = 67, sdef = 85, spd = 116, total = 480}, generation = 5},Pokemon {name = "Petilil", types = [Grass], dex = 548, stats = Stats {hp = 45, atk = 35, def = 50, satk = 35, sdef = 50, spd = 30, total = 280}, generation = 5},Pokemon {name = "Lilligant", types = [Grass], dex = 549, stats = Stats {hp = 70, atk = 60, def = 75, satk = 60, sdef = 75, spd = 90, total = 480}, generation = 5},Pokemon {name = "Basculin", types = [Water], dex = 550, stats = Stats {hp = 70, atk = 92, def = 65, satk = 92, sdef = 65, spd = 98, total = 460}, generation = 5},Pokemon {name = "Sandile", types = [Ground,Dark], dex = 551, stats = Stats {hp = 50, atk = 72, def = 35, satk = 72, sdef = 35, spd = 65, total = 292}, generation = 5},Pokemon {name = "Krokorok", types = [Ground,Dark], dex = 552, stats = Stats {hp = 60, atk = 82, def = 45, satk = 82, sdef = 45, spd = 74, total = 351}, generation = 5},Pokemon {name = "Krookodile", types = [Ground,Dark], dex = 553, stats = Stats {hp = 90, atk = 117, def = 70, satk = 117, sdef = 70, spd = 92, total = 504}, generation = 5},Pokemon {name = "Darumaka", types = [Fire], dex = 554, stats = Stats {hp = 70, atk = 90, def = 45, satk = 90, sdef = 45, spd = 50, total = 315}, generation = 5},Pokemon {name = "Darmanitan", types = [Fire], dex = 555, stats = Stats {hp = 105, atk = 140, def = 55, satk = 140, sdef = 55, spd = 95, total = 480}, generation = 5},Pokemon {name = "Maractus", types = [Grass], dex = 556, stats = Stats {hp = 75, atk = 86, def = 67, satk = 86, sdef = 67, spd = 60, total = 461}, generation = 5},Pokemon {name = "Dwebble", types = [Bug,Rock], dex = 557, stats = Stats {hp = 50, atk = 65, def = 85, satk = 65, sdef = 85, spd = 55, total = 325}, generation = 5},Pokemon {name = "Crustle", types = [Bug,Rock], dex = 558, stats = Stats {hp = 70, atk = 95, def = 125, satk = 95, sdef = 125, spd = 45, total = 475}, generation = 5},Pokemon {name = "Scraggy", types = [Dark,Fighting], dex = 559, stats = Stats {hp = 50, atk = 75, def = 70, satk = 75, sdef = 70, spd = 48, total = 348}, generation = 5},Pokemon {name = "Scrafty", types = [Dark,Fighting], dex = 560, stats = Stats {hp = 65, atk = 90, def = 115, satk = 90, sdef = 115, spd = 58, total = 488}, generation = 5},Pokemon {name = "Sigilyph", types = [Psychic,Flying], dex = 561, stats = Stats {hp = 75, atk = 58, def = 80, satk = 58, sdef = 80, spd = 97, total = 493}, generation = 5},Pokemon {name = "Yamask", types = [Ghost], dex = 562, stats = Stats {hp = 38, atk = 30, def = 85, satk = 30, sdef = 85, spd = 30, total = 303}, generation = 5},Pokemon {name = "Cofagrigus", types = [Ghost], dex = 563, stats = Stats {hp = 58, atk = 50, def = 145, satk = 50, sdef = 145, spd = 30, total = 483}, generation = 5},Pokemon {name = "Tirtouga", types = [Water,Rock], dex = 564, stats = Stats {hp = 54, atk = 78, def = 103, satk = 78, sdef = 103, spd = 22, total = 355}, generation = 5},Pokemon {name = "Carracosta", types = [Water,Rock], dex = 565, stats = Stats {hp = 74, atk = 108, def = 133, satk = 108, sdef = 133, spd = 32, total = 495}, generation = 5},Pokemon {name = "Archen", types = [Rock,Flying], dex = 566, stats = Stats {hp = 55, atk = 112, def = 45, satk = 112, sdef = 45, spd = 70, total = 401}, generation = 5},Pokemon {name = "Archeops", types = [Rock,Flying], dex = 567, stats = Stats {hp = 75, atk = 140, def = 65, satk = 140, sdef = 65, spd = 110, total = 567}, generation = 5},Pokemon {name = "Trubbish", types = [Poison], dex = 568, stats = Stats {hp = 50, atk = 50, def = 62, satk = 50, sdef = 62, spd = 65, total = 329}, generation = 5},Pokemon {name = "Garbodor", types = [Poison], dex = 569, stats = Stats {hp = 80, atk = 95, def = 82, satk = 95, sdef = 82, spd = 75, total = 474}, generation = 5},Pokemon {name = "Zorua", types = [Dark], dex = 570, stats = Stats {hp = 40, atk = 65, def = 40, satk = 65, sdef = 40, spd = 65, total = 330}, generation = 5},Pokemon {name = "Zoroark", types = [Dark], dex = 571, stats = Stats {hp = 60, atk = 105, def = 60, satk = 105, sdef = 60, spd = 105, total = 510}, generation = 5},Pokemon {name = "Minccino", types = [Normal], dex = 572, stats = Stats {hp = 55, atk = 50, def = 40, satk = 50, sdef = 40, spd = 75, total = 300}, generation = 5},Pokemon {name = "Cinccino", types = [Normal], dex = 573, stats = Stats {hp = 75, atk = 95, def = 60, satk = 95, sdef = 60, spd = 115, total = 470}, generation = 5},Pokemon {name = "Gothita", types = [Psychic], dex = 574, stats = Stats {hp = 45, atk = 30, def = 50, satk = 30, sdef = 50, spd = 45, total = 290}, generation = 5},Pokemon {name = "Gothorita", types = [Psychic], dex = 575, stats = Stats {hp = 60, atk = 45, def = 70, satk = 45, sdef = 70, spd = 55, total = 390}, generation = 5},Pokemon {name = "Gothitelle", types = [Psychic], dex = 576, stats = Stats {hp = 70, atk = 55, def = 95, satk = 55, sdef = 95, spd = 65, total = 490}, generation = 5},Pokemon {name = "Solosis", types = [Psychic], dex = 577, stats = Stats {hp = 45, atk = 30, def = 40, satk = 30, sdef = 40, spd = 20, total = 290}, generation = 5},Pokemon {name = "Duosion", types = [Psychic], dex = 578, stats = Stats {hp = 65, atk = 40, def = 50, satk = 40, sdef = 50, spd = 30, total = 370}, generation = 5},Pokemon {name = "Reuniclus", types = [Psychic], dex = 579, stats = Stats {hp = 110, atk = 65, def = 75, satk = 65, sdef = 75, spd = 30, total = 490}, generation = 5},Pokemon {name = "Ducklett", types = [Water,Flying], dex = 580, stats = Stats {hp = 62, atk = 44, def = 50, satk = 44, sdef = 50, spd = 55, total = 305}, generation = 5},Pokemon {name = "Swanna", types = [Water,Flying], dex = 581, stats = Stats {hp = 75, atk = 87, def = 63, satk = 87, sdef = 63, spd = 98, total = 473}, generation = 5},Pokemon {name = "Vanillite", types = [Ice], dex = 582, stats = Stats {hp = 36, atk = 50, def = 50, satk = 50, sdef = 50, spd = 44, total = 305}, generation = 5},Pokemon {name = "Vanillish", types = [Ice], dex = 583, stats = Stats {hp = 51, atk = 65, def = 65, satk = 65, sdef = 65, spd = 59, total = 395}, generation = 5},Pokemon {name = "Vanilluxe", types = [Ice], dex = 584, stats = Stats {hp = 71, atk = 95, def = 85, satk = 95, sdef = 85, spd = 79, total = 535}, generation = 5},Pokemon {name = "Deerling", types = [Normal,Grass], dex = 585, stats = Stats {hp = 60, atk = 60, def = 50, satk = 60, sdef = 50, spd = 75, total = 335}, generation = 5},Pokemon {name = "Sawsbuck", types = [Normal,Grass], dex = 586, stats = Stats {hp = 80, atk = 100, def = 70, satk = 100, sdef = 70, spd = 95, total = 475}, generation = 5},Pokemon {name = "Emolga", types = [Electric,Flying], dex = 587, stats = Stats {hp = 55, atk = 75, def = 60, satk = 75, sdef = 60, spd = 103, total = 428}, generation = 5},Pokemon {name = "Karrablast", types = [Bug], dex = 588, stats = Stats {hp = 50, atk = 75, def = 45, satk = 75, sdef = 45, spd = 60, total = 315}, generation = 5},Pokemon {name = "Escavalier", types = [Bug,Steel], dex = 589, stats = Stats {hp = 70, atk = 135, def = 105, satk = 135, sdef = 105, spd = 20, total = 495}, generation = 5},Pokemon {name = "Foongus", types = [Grass,Poison], dex = 590, stats = Stats {hp = 69, atk = 55, def = 45, satk = 55, sdef = 45, spd = 15, total = 294}, generation = 5},Pokemon {name = "Amoonguss", types = [Grass,Poison], dex = 591, stats = Stats {hp = 114, atk = 85, def = 70, satk = 85, sdef = 70, spd = 30, total = 464}, generation = 5},Pokemon {name = "Frillish", types = [Water,Ghost], dex = 592, stats = Stats {hp = 55, atk = 40, def = 50, satk = 40, sdef = 50, spd = 40, total = 335}, generation = 5},Pokemon {name = "Jellicent", types = [Water,Ghost], dex = 593, stats = Stats {hp = 100, atk = 60, def = 70, satk = 60, sdef = 70, spd = 60, total = 480}, generation = 5},Pokemon {name = "Alomomola", types = [Water], dex = 594, stats = Stats {hp = 165, atk = 75, def = 80, satk = 75, sdef = 80, spd = 65, total = 470}, generation = 5},Pokemon {name = "Joltik", types = [Bug,Electric], dex = 595, stats = Stats {hp = 50, atk = 47, def = 50, satk = 47, sdef = 50, spd = 65, total = 319}, generation = 5},Pokemon {name = "Galvantula", types = [Bug,Electric], dex = 596, stats = Stats {hp = 70, atk = 77, def = 60, satk = 77, sdef = 60, spd = 108, total = 472}, generation = 5},Pokemon {name = "Ferroseed", types = [Grass,Steel], dex = 597, stats = Stats {hp = 44, atk = 50, def = 91, satk = 50, sdef = 91, spd = 10, total = 305}, generation = 5},Pokemon {name = "Ferrothorn", types = [Grass,Steel], dex = 598, stats = Stats {hp = 74, atk = 94, def = 132, satk = 94, sdef = 132, spd = 20, total = 490}, generation = 5},Pokemon {name = "Klink", types = [Steel], dex = 599, stats = Stats {hp = 50, atk = 45, def = 70, satk = 45, sdef = 70, spd = 30, total = 300}, generation = 5},Pokemon {name = "Klang", types = [Steel], dex = 600, stats = Stats {hp = 60, atk = 80, def = 95, satk = 80, sdef = 95, spd = 50, total = 440}, generation = 5},Pokemon {name = "Klinklang", types = [Steel], dex = 601, stats = Stats {hp = 60, atk = 100, def = 115, satk = 100, sdef = 115, spd = 90, total = 520}, generation = 5},Pokemon {name = "Tynamo", types = [Electric], dex = 602, stats = Stats {hp = 35, atk = 55, def = 40, satk = 55, sdef = 40, spd = 60, total = 275}, generation = 5},Pokemon {name = "Eelektrik", types = [Electric], dex = 603, stats = Stats {hp = 65, atk = 85, def = 70, satk = 85, sdef = 70, spd = 40, total = 405}, generation = 5},Pokemon {name = "Eelektross", types = [Electric], dex = 604, stats = Stats {hp = 85, atk = 115, def = 80, satk = 115, sdef = 80, spd = 50, total = 515}, generation = 5},Pokemon {name = "Elgyem", types = [Psychic], dex = 605, stats = Stats {hp = 55, atk = 55, def = 55, satk = 55, sdef = 55, spd = 30, total = 335}, generation = 5},Pokemon {name = "Beheeyem", types = [Psychic], dex = 606, stats = Stats {hp = 75, atk = 75, def = 75, satk = 75, sdef = 75, spd = 40, total = 485}, generation = 5},Pokemon {name = "Litwick", types = [Ghost,Fire], dex = 607, stats = Stats {hp = 50, atk = 30, def = 55, satk = 30, sdef = 55, spd = 20, total = 275}, generation = 5},Pokemon {name = "Lampent", types = [Ghost,Fire], dex = 608, stats = Stats {hp = 60, atk = 40, def = 60, satk = 40, sdef = 60, spd = 55, total = 370}, generation = 5},Pokemon {name = "Chandelure", types = [Ghost,Fire], dex = 609, stats = Stats {hp = 60, atk = 55, def = 90, satk = 55, sdef = 90, spd = 80, total = 520}, generation = 5},Pokemon {name = "Axew", types = [Dragon], dex = 610, stats = Stats {hp = 46, atk = 87, def = 60, satk = 87, sdef = 60, spd = 57, total = 320}, generation = 5},Pokemon {name = "Fraxure", types = [Dragon], dex = 611, stats = Stats {hp = 66, atk = 117, def = 70, satk = 117, sdef = 70, spd = 67, total = 410}, generation = 5},Pokemon {name = "Haxorus", types = [Dragon], dex = 612, stats = Stats {hp = 76, atk = 147, def = 90, satk = 147, sdef = 90, spd = 97, total = 540}, generation = 5},Pokemon {name = "Cubchoo", types = [Ice], dex = 613, stats = Stats {hp = 55, atk = 70, def = 40, satk = 70, sdef = 40, spd = 40, total = 305}, generation = 5},Pokemon {name = "Beartic", types = [Ice], dex = 614, stats = Stats {hp = 95, atk = 110, def = 80, satk = 110, sdef = 80, spd = 50, total = 485}, generation = 5},Pokemon {name = "Cryogonal", types = [Ice], dex = 615, stats = Stats {hp = 70, atk = 50, def = 30, satk = 50, sdef = 30, spd = 105, total = 485}, generation = 5},Pokemon {name = "Shelmet", types = [Bug], dex = 616, stats = Stats {hp = 50, atk = 40, def = 85, satk = 40, sdef = 85, spd = 25, total = 305}, generation = 5},Pokemon {name = "Accelgor", types = [Bug], dex = 617, stats = Stats {hp = 80, atk = 70, def = 40, satk = 70, sdef = 40, spd = 145, total = 495}, generation = 5},Pokemon {name = "Stunfisk", types = [Ground,Electric], dex = 618, stats = Stats {hp = 109, atk = 66, def = 84, satk = 66, sdef = 84, spd = 32, total = 471}, generation = 5},Pokemon {name = "Mienfoo", types = [Fighting], dex = 619, stats = Stats {hp = 45, atk = 85, def = 50, satk = 85, sdef = 50, spd = 65, total = 350}, generation = 5},Pokemon {name = "Mienshao", types = [Fighting], dex = 620, stats = Stats {hp = 65, atk = 125, def = 60, satk = 125, sdef = 60, spd = 105, total = 510}, generation = 5},Pokemon {name = "Druddigon", types = [Dragon], dex = 621, stats = Stats {hp = 77, atk = 120, def = 90, satk = 120, sdef = 90, spd = 48, total = 485}, generation = 5},Pokemon {name = "Golett", types = [Ground,Ghost], dex = 622, stats = Stats {hp = 59, atk = 74, def = 50, satk = 74, sdef = 50, spd = 35, total = 303}, generation = 5},Pokemon {name = "Golurk", types = [Ground,Ghost], dex = 623, stats = Stats {hp = 89, atk = 124, def = 80, satk = 124, sdef = 80, spd = 55, total = 483}, generation = 5},Pokemon {name = "Pawniard", types = [Dark,Steel], dex = 624, stats = Stats {hp = 45, atk = 85, def = 70, satk = 85, sdef = 70, spd = 60, total = 340}, generation = 5},Pokemon {name = "Bisharp", types = [Dark,Steel], dex = 625, stats = Stats {hp = 65, atk = 125, def = 100, satk = 125, sdef = 100, spd = 70, total = 490}, generation = 5},Pokemon {name = "Bouffalant", types = [Normal], dex = 626, stats = Stats {hp = 95, atk = 110, def = 95, satk = 110, sdef = 95, spd = 55, total = 490}, generation = 5},Pokemon {name = "Rufflet", types = [Normal,Flying], dex = 627, stats = Stats {hp = 70, atk = 83, def = 50, satk = 83, sdef = 50, spd = 60, total = 350}, generation = 5},Pokemon {name = "Braviary", types = [Normal,Flying], dex = 628, stats = Stats {hp = 100, atk = 123, def = 75, satk = 123, sdef = 75, spd = 80, total = 510}, generation = 5},Pokemon {name = "Vullaby", types = [Dark,Flying], dex = 629, stats = Stats {hp = 70, atk = 55, def = 75, satk = 55, sdef = 75, spd = 60, total = 370}, generation = 5},Pokemon {name = "Mandibuzz", types = [Dark,Flying], dex = 630, stats = Stats {hp = 110, atk = 65, def = 105, satk = 65, sdef = 105, spd = 80, total = 510}, generation = 5},Pokemon {name = "Heatmor", types = [Fire], dex = 631, stats = Stats {hp = 85, atk = 97, def = 66, satk = 97, sdef = 66, spd = 65, total = 484}, generation = 5},Pokemon {name = "Durant", types = [Bug,Steel], dex = 632, stats = Stats {hp = 58, atk = 109, def = 112, satk = 109, sdef = 112, spd = 109, total = 484}, generation = 5},Pokemon {name = "Deino", types = [Dark,Dragon], dex = 633, stats = Stats {hp = 52, atk = 65, def = 50, satk = 65, sdef = 50, spd = 38, total = 300}, generation = 5},Pokemon {name = "Zweilous", types = [Dark,Dragon], dex = 634, stats = Stats {hp = 72, atk = 85, def = 70, satk = 85, sdef = 70, spd = 58, total = 420}, generation = 5},Pokemon {name = "Hydreigon", types = [Dark,Dragon], dex = 635, stats = Stats {hp = 92, atk = 105, def = 90, satk = 105, sdef = 90, spd = 98, total = 600}, generation = 5},Pokemon {name = "Larvesta", types = [Bug,Fire], dex = 636, stats = Stats {hp = 55, atk = 85, def = 55, satk = 85, sdef = 55, spd = 60, total = 360}, generation = 5},Pokemon {name = "Volcarona", types = [Bug,Fire], dex = 637, stats = Stats {hp = 80, atk = 60, def = 65, satk = 60, sdef = 65, spd = 100, total = 550}, generation = 5},Pokemon {name = "Cobalion", types = [Steel,Fighting], dex = 638, stats = Stats {hp = 91, atk = 90, def = 129, satk = 90, sdef = 129, spd = 108, total = 580}, generation = 5},Pokemon {name = "Terrakion", types = [Rock,Fighting], dex = 639, stats = Stats {hp = 91, atk = 129, def = 90, satk = 129, sdef = 90, spd = 108, total = 580}, generation = 5},Pokemon {name = "Virizion", types = [Grass,Fighting], dex = 640, stats = Stats {hp = 91, atk = 90, def = 72, satk = 90, sdef = 72, spd = 108, total = 580}, generation = 5},Pokemon {name = "Tornadus", types = [Flying], dex = 641, stats = Stats {hp = 79, atk = 115, def = 70, satk = 115, sdef = 70, spd = 111, total = 580}, generation = 5},Pokemon {name = "Thundurus", types = [Electric,Flying], dex = 642, stats = Stats {hp = 79, atk = 115, def = 70, satk = 115, sdef = 70, spd = 111, total = 580}, generation = 5},Pokemon {name = "Reshiram", types = [Dragon,Fire], dex = 643, stats = Stats {hp = 100, atk = 120, def = 100, satk = 120, sdef = 100, spd = 90, total = 680}, generation = 5},Pokemon {name = "Zekrom", types = [Dragon,Electric], dex = 644, stats = Stats {hp = 100, atk = 150, def = 120, satk = 150, sdef = 120, spd = 90, total = 680}, generation = 5},Pokemon {name = "Landorus", types = [Ground,Flying], dex = 645, stats = Stats {hp = 89, atk = 125, def = 90, satk = 125, sdef = 90, spd = 101, total = 600}, generation = 5},Pokemon {name = "Kyurem", types = [Dragon,Ice], dex = 646, stats = Stats {hp = 125, atk = 130, def = 90, satk = 130, sdef = 90, spd = 95, total = 660}, generation = 5},Pokemon {name = "Keldeo", types = [Water,Fighting], dex = 647, stats = Stats {hp = 91, atk = 72, def = 90, satk = 72, sdef = 90, spd = 108, total = 580}, generation = 5},Pokemon {name = "Meloetta", types = [Normal,Psychic], dex = 648, stats = Stats {hp = 100, atk = 77, def = 77, satk = 77, sdef = 77, spd = 90, total = 600}, generation = 5},Pokemon {name = "Genesect", types = [Bug,Steel], dex = 649, stats = Stats {hp = 71, atk = 120, def = 95, satk = 120, sdef = 95, spd = 99, total = 600}, generation = 5}]
