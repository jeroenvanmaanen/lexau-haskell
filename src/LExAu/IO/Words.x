{
{-# LANGUAGE BangPatterns #-}
module LExAu.IO.Words (alexScanTokens) where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

  $white+                       ;
--  $digit+ ":" $digit+           { id }
--  $digit+ / [^:]                { id }
  $digit                        { id }
  $alpha+                       { id }
  [^ $digit $white $alpha ] / [$white]                          { (++ " ") }
  [$white]  ^ [^ $digit $white $alpha ] / [ \n [^$white] ]      { (' ' :) }
  [^ $digit $white $alpha ]     { id }
