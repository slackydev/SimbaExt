const
  CharsLower      = 'abcdefghijklmnopqrstuvwxyz';
  CharsUpper      = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  CharsLetters    = CharsLower + CharsUpper;
  CharsDigits     = '0123456789';
  CharsSymbols    = '!"#$%&'+#39+'()*+,-./:;<=>?@[\]^_`{|}~';
  CharsWhitespace = #9#10#11#12#13#32;
  CharsAlphaNum   = CharsLetters + CharsDigits;
  CharsAll        = CharsLetters + CharsDigits + CharsSymbols + CharsWhitespace; 