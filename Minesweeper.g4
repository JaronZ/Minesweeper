/*
 * This file is only used to work out what functions to implement for the parser.
 * See src/Parser for the actual implementation.
 */
grammar Minesweeper;

options {
    caseInsensitive = true;
}

// --- Lexer ---

// Keywords for actions
CLICK: 'click';
FLAG: 'flag';
UNKNOWN: 'unknown';

// Cell Identifiers
CELL_ID: [A-Z] [0-9];

// Whitespace
WS: [ \t\r\n] -> skip;

// --- Parser ---

program: action CELL_ID EOF;
action: CLICK | FLAG | UNKNOWN;
