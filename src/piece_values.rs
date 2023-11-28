pub static mut PIECE_VALUES: [[[i16; 64]; 7]; 2] = [
    [
        [
            65, 65, 65, 65, 65, 65, 65, 65, 75, 75, 75, 75, 75, 75, 75, 75, 85, 85, 85, 85, 85, 85,
            85, 85, 95, 95, 95, 95, 95, 95, 95, 95, 105, 105, 105, 105, 105, 105, 105, 105, 115,
            115, 115, 115, 115, 115, 115, 115, 125, 125, 125, 125, 125, 125, 125, 125, 135, 135,
            135, 135, 135, 135, 135, 135,
        ],
        [
            265, 265, 265, 265, 265, 265, 265, 265, 275, 275, 275, 275, 275, 275, 275, 275, 285,
            285, 285, 285, 285, 285, 285, 285, 295, 295, 295, 295, 295, 295, 295, 295, 305, 305,
            305, 305, 305, 305, 305, 305, 315, 315, 315, 315, 315, 315, 315, 315, 325, 325, 325,
            325, 325, 325, 325, 325, 335, 335, 335, 335, 335, 335, 335, 335,
        ],
        [
            265, 265, 265, 265, 265, 265, 265, 265, 275, 275, 275, 275, 275, 275, 275, 275, 285,
            285, 285, 285, 285, 285, 285, 285, 295, 295, 295, 295, 295, 295, 295, 295, 305, 305,
            305, 305, 305, 305, 305, 305, 315, 315, 315, 315, 315, 315, 315, 315, 325, 325, 325,
            325, 325, 325, 325, 325, 335, 335, 335, 335, 335, 335, 335, 335,
        ],
        [
            465, 465, 465, 465, 465, 465, 465, 465, 475, 475, 475, 475, 475, 475, 475, 475, 485,
            485, 485, 485, 485, 485, 485, 485, 495, 495, 495, 495, 495, 495, 495, 495, 505, 505,
            505, 505, 505, 505, 505, 505, 515, 515, 515, 515, 515, 515, 515, 515, 525, 525, 525,
            525, 525, 525, 525, 525, 535, 535, 535, 535, 535, 535, 535, 535,
        ],
        [
            465, 465, 465, 465, 465, 465, 465, 465, 475, 475, 475, 475, 475, 475, 475, 475, 485,
            485, 485, 485, 485, 485, 485, 485, 495, 495, 495, 495, 495, 495, 495, 495, 505, 505,
            505, 505, 505, 505, 505, 505, 515, 515, 515, 515, 515, 515, 515, 515, 525, 525, 525,
            525, 525, 525, 525, 525, 535, 535, 535, 535, 535, 535, 535, 535,
        ],
        [
            900, 900, 900, 900, 900, 900, 900, 900, 900, 900, 900, 900, 900, 900, 900, 900, 900,
            900, 900, 900, 900, 900, 900, 900, 900, 900, 900, 900, 900, 900, 900, 900, 900, 900,
            900, 900, 900, 900, 900, 900, 900, 900, 900, 900, 900, 900, 900, 900, 900, 900, 900,
            900, 900, 900, 900, 900, 900, 900, 900, 900, 900, 900, 900, 900,
        ],
        [
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0,
        ],
    ],
    [
        [
            135, 135, 135, 135, 135, 135, 135, 135, 125, 125, 125, 125, 125, 125, 125, 125, 115,
            115, 115, 115, 115, 115, 115, 115, 105, 105, 105, 105, 105, 105, 105, 105, 95, 95, 95,
            95, 95, 95, 95, 95, 85, 85, 85, 85, 85, 85, 85, 85, 75, 75, 75, 75, 75, 75, 75, 75, 65,
            65, 65, 65, 65, 65, 65, 65,
        ],
        [
            335, 335, 335, 335, 335, 335, 335, 335, 325, 325, 325, 325, 325, 325, 325, 325, 315,
            315, 315, 315, 315, 315, 315, 315, 305, 305, 305, 305, 305, 305, 305, 305, 295, 295,
            295, 295, 295, 295, 295, 295, 285, 285, 285, 285, 285, 285, 285, 285, 275, 275, 275,
            275, 275, 275, 275, 275, 265, 265, 265, 265, 265, 265, 265, 265,
        ],
        [
            335, 335, 335, 335, 335, 335, 335, 335, 325, 325, 325, 325, 325, 325, 325, 325, 315,
            315, 315, 315, 315, 315, 315, 315, 305, 305, 305, 305, 305, 305, 305, 305, 295, 295,
            295, 295, 295, 295, 295, 295, 285, 285, 285, 285, 285, 285, 285, 285, 275, 275, 275,
            275, 275, 275, 275, 275, 265, 265, 265, 265, 265, 265, 265, 265,
        ],
        [
            535, 535, 535, 535, 535, 535, 535, 535, 525, 525, 525, 525, 525, 525, 525, 525, 515,
            515, 515, 515, 515, 515, 515, 515, 505, 505, 505, 505, 505, 505, 505, 505, 495, 495,
            495, 495, 495, 495, 495, 495, 485, 485, 485, 485, 485, 485, 485, 485, 475, 475, 475,
            475, 475, 475, 475, 475, 465, 465, 465, 465, 465, 465, 465, 465,
        ],
        [
            535, 535, 535, 535, 535, 535, 535, 535, 525, 525, 525, 525, 525, 525, 525, 525, 515,
            515, 515, 515, 515, 515, 515, 515, 505, 505, 505, 505, 505, 505, 505, 505, 495, 495,
            495, 495, 495, 495, 495, 495, 485, 485, 485, 485, 485, 485, 485, 485, 475, 475, 475,
            475, 475, 475, 475, 475, 465, 465, 465, 465, 465, 465, 465, 465,
        ],
        [
            900, 900, 900, 900, 900, 900, 900, 900, 900, 900, 900, 900, 900, 900, 900, 900, 900,
            900, 900, 900, 900, 900, 900, 900, 900, 900, 900, 900, 900, 900, 900, 900, 900, 900,
            900, 900, 900, 900, 900, 900, 900, 900, 900, 900, 900, 900, 900, 900, 900, 900, 900,
            900, 900, 900, 900, 900, 900, 900, 900, 900, 900, 900, 900, 900,
        ],
        [
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0,
        ],
    ],
];