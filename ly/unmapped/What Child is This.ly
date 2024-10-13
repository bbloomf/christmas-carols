\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"What Child is This?"}}
  poet = \markup\oldStyleNum"William C. Dix (1837–1898)"
  composer = \markup\oldStyleNum"16th Century English Air"
  arranger = \markup\oldStyleNum"Arranged by Sir John Stainer (1840–1901)"
  tagline = \markup { "from" \italic {Christmas Carols, New and Old}}
}
\paper {
  %print-all-headers = ##f
  paper-height = 9\in
  paper-width = 6\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  %system-system-spacing =
  %  #'((basic-distance . 0)
  %     (minimum-distance . 0)
  %     (padding . -0.35)
  %     (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #041
  print-first-page-number = ##t
  headerLine = \markup{\override #'(font-name . "Garamond Premier Pro") \smallCapsOldStyle christmas}
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #8.5
     \combine 
        \fill-line{"" \oldStylePageNum"" }
        \fill-line{\headerLine }
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #8.5
     \combine
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 15) \paper{ #(define fonts (make-pango-font-tree "GoudyOlSt BT" "Garamond Premier Pro" "Garamond Premier Pro" (/ 15 20))) }

global = {
  \key g \major
  \time 6/8
  \autoBeamOff
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(-1 . 1)
  \mergeDifferentlyHeadedOn
  \mergeDifferentlyDottedOn
}

sopMusic = \relative c' {
  \partial 8 e8 |
  g4 a8 b8.[ c16] b8 |
  a4 fis8 d8.[ e16] fis8 |
  g4 e8 e8.[ dis16] e8 |
  
  fis4. b,4 \bar""\break e8 |
  g4 a8 b8.[ c16] b8 |
  a4 fis8 d8.[ e16] fis8 |
  g8.[ fis16] e8 dis8.[ cis16] dis8 |
  e4. e |
  
  d'4. d8.[ cis16] b8 |
  a4 fis8 d8.[ e16] fis8 |
  g4 e8 e8.[ dis16] e8 |
  fis4 dis8 b4. |
  
  d'4. d8.[ cis16] b8 |
  a4 fis8 d8.[ e16] fis8 |
  g8.[ fis16] e8 dis8.[ cis16] dis8 |
  \partial 8*5 e4. e4 \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  b8 |
  e4 d8 d4 g8 |
  fis4 d8 d4 c8 |
  b4 b8 a4 e'8 |
  
  dis4. b4 b8 |
  e4 d8 d4 g8 |
  fis4 d8 d4 c8 |
  b4 c8 b4 b8 |
  b4. b |
  
  fis' b8.[ a16] g8 |
  fis4 d8 d4 c8 |
  b4 b8 a4 e'8 |
  dis4 b8 b4. |
  
  fis'4. b8.[ a16] g8 |
  fis4 d8 d4 c8 |
  b4 c8 b4 b8 |
  b4. b4 \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsV
  \set stanza = #"1."
  What Child is this, Who, laid
  \set associatedVoice = "altos"
  to rest,
  \unset associatedVoice
  On Ma -- ry’s lap __ is sleep -- ing?
  Whom an -- gels greet with
  \set associatedVoice = "altos"
  an -- thems sweet,
  \unset associatedVoice
  While shep -- herds watch are keep -- ing?
  
  This, this __ is
  \set associatedVoice = "altos"
  Christ the King;
  \unset associatedVoice
  Whom shep -- herds guard and an -- gels sing:
  \raiseLyrics
  Haste, haste __ to
  \set associatedVoice = "altos"
  bring Him laud,
  \unset associatedVoice
  The Babe, the Son __ of Ma -- ry!
}
altoWordsII = {
  \dropLyricsV
  \set stanza = \markup{\dynamic"  mf " "2."}
  \lyricmode {
    Why lies He in __ such
    \set associatedVoice = "altos"
    mean es -- tate,
    \unset associatedVoice
    Where ox and ass __ are feed -- ing?
    Good Chris -- tian, fear: for
    \set associatedVoice = "altos"
    sin -- ners here
    \unset associatedVoice
    The si -- lent Word is plead -- ing:
  }
  \set stanza = \markup\dynamic"  ff "
  \lyricmode {
    Nails, spear,
%8.5x11 __
    shall
    \set associatedVoice = "altos"
    pierce Him through,
    \unset associatedVoice
    The Cross be borne, for me, for you:
  \raiseLyrics
    Hail, hail __ the
    \set associatedVoice = "altos"
    Word made flesh,
    \unset associatedVoice
    The Babe, the Son __ of Ma -- ry!
  }
}
altoWordsIII = \lyricmode {
  \dropLyricsV
  \set stanza = #"3."
  So bring Him in -- cense,
  \set associatedVoice = "altos"
  gold, and myrrh,
  \unset associatedVoice
  Come peas -- ant, king,
%8.5x11 __
  to own Him;
  The King of kings,
  \set associatedVoice = "altos"
  sal -- va -- tion brings;
  \unset associatedVoice
  Let lov -- ing hearts en -- throne Him.
  
  Raise, raise __ the
  \set associatedVoice = "altos"
  song on high
  \unset associatedVoice
  The Vir -- gin sings her lul -- la -- by:
  \raiseLyrics
  Joy, joy __ for
  \set associatedVoice = "altos"
  Christ is born,
  \unset associatedVoice
  The Babe, the Son __ of Ma -- ry!
}
altoWordsIV = \lyricmode {
}
altoWordsV = \lyricmode {
}
altoWordsVI = \lyricmode {
}
tenorMusic = \relative c' {
  g8 |
  b4 a8 g4 b8 |
  d4 a8 fis4 a8 |
  g4 g8 e4 e8 |
  
  b'4. b4 g8 |
  b4 a8 g4 b8 |
  d4 a8 fis4 a8 |
  g4 a8 fis4 fis8 |
  g4. g |
  
  b4. d4 d8 |
  d4 a8 fis4 a8 |
  g4 g8 a4 e8 |
  b'4 fis8 b4. |
  
  b d4 d8 |
  d4 a8 fis4 a8 |
  g4 a8 fis4 fis8 |
  g4. g4 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  e8 |
  e4 fis8 g4 g8 |
  d4 d8 d4 dis8 |
  e4 e8 c4 c8 |
  
  b4. b'4 e,8 |
  e4 fis8 g4 g8 |
  d4 d8 d4 dis8 |
  e4 a,8 b4 b8 |
  e4. e |
  
  b' g4 g8 |
  d4 d8 d4 dis8 |
  e4 e8 c4 c8 |
  b4 b8 b4. |
  
  b' g4 g8 |
  d4 d8 d4 dis8 |
  e4 a,8 b4 b8 |
  e4. e4 \bar "|."
}
bassWords = \lyricmode {

}

pianoRH = \relative c' {
  
}
pianoLH = \relative c' {
  
}

\score {
  <<
   \new ChoirStaff <<
%    \new Lyrics = sopranos \with { \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) }
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
    \context {
      \Score
      %\override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      %\override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
    }
    \context {
      % Remove all empty staves
      % \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
    }
    \context {
      \Lyrics
      \override LyricText #'X-offset = #center-on-word
    }
  }
  \midi {
    \tempo 4 = 60
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}
