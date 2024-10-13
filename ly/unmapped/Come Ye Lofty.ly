\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"Come Ye Lofty"}}
  poet = \markup\oldStyleNum"Rev. Archer Gurney (1820–1887)"
  composer = \markup\oldStyleNum"G.J. Elvey (1816–1893)"
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
  first-page-number = #117
  print-first-page-number = ##t
  headerLine = \markup{\override #'(font-name . "Garamond Premier Pro") \smallCapsOldStyle"christmas"}
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #8.5
     \combine 
        \fill-line{"" \if \should-print-page-number
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #8.5
     \combine
        \if \should-print-page-number
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 15) \paper{ #(define fonts (make-pango-font-tree "GoudyOlSt BT" "Garamond Premier Pro" "Garamond Premier Pro" (/ 15 20))) }
global = {
  \key d \major
  \time 4/4
  \mergeDifferentlyDottedOn
  \mergeDifferentlyHeadedOn
}

sopMusic = \relative c' {
  d4. d8 g4 fis |
  d4. d8 b'4 a |
  d4. d8 cis4 b |
  a fis fis( e) |
  
  d4. d8 g4 fis |
  d4. d8 b'4 a |
  d4. d8 d4 cis |
  b ais b2 |
  
  b4. b8 b4 a |
  d4. d8 d4 cis |
  d4. d8 cis4 b |
  a gis a2 |
  
  g4. g8 g4 fis |
  c'4. c8 c4 b4 |
  e4. d8 cis4 d |
  fis, e d2 \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  d4. d8 d4 d |
  d4. d8 d4 d |
  d4. d8 e4 e |
  a, d d( cis) |
  
  d4. d8 d4 d |
  d4. d8 d4 d |
  d4. d8 g4 g |
  fis fis fis2 |
  
  g4. g8 g4 fis |
  fis4. fis8 fis4 fis |
  fis e e fis |
  e e e2 |
  
  d4 e d d |
  c d d d |
  g g e d |
  d cis d2 \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsIX
  \set stanza = #"1. "
  Come ye lof -- ty, come ye low -- ly,
  Let your songs of glad -- ness ring;
  In a sta -- ble lies the Ho -- ly,
  In a man -- ger rests the King:
  See in Ma -- ry’s arms re -- pos -- ing
  Christ by high -- est Heav’n a -- dored:
  Come, your cir -- cle round Him clos -- ing,
  Pi -- ous hearts that love the Lord.
}
altoWordsII = \lyricmode {
  \dropLyricsIX
%\markup\italic
  \set stanza = #"2. "
  Come ye poor, no pomp of sta -- tion
  Robes the Child your hearts a -- dore:
  He, the Lord of all sal -- va -- tion,
  Shares your want, is weak and poor:
  Ox -- en, round a -- bout be -- hold them;
  Raft -- ers na -- ked, cold, and bare,
  See the Shep -- herds, God has told them
  That the Prince of Life lies there.
}
altoWordsIII = \lyricmode {
  \dropLyricsIX
  \set stanza = #"3. "
  Come ye chil -- dren blithe and mer -- ry,
  This one Child your mod -- el make;
  Christ -- mas hol -- ly, leaf, and ber -- ry,
  All be prized for His dear sake:
  Come ye gen -- tle hearts and ten -- der.
  Come ye spi -- rits keen and bold;
  All in all your hom -- age ren -- der,
  Weak and might -- y, young and old.
}
altoWordsIV = \lyricmode {
  \dropLyricsIX
  \set stanza = #"4. "
  High a -- bove a star is shin -- ing,
  And the wise men haste from far: __
  Come glad hearts, and spi -- rits pin -- ing:
  For you all has ris’n the star.
  Let us bring our poor o -- bla -- tions,
  Thanks and love, and faith and praise;
  Come ye peo -- ple, come ye na -- tions,
  All in all draw nigh to gaze.
}
altoWordsV = \lyricmode {
  \dropLyricsIX
  \set stanza = #"5. "
  Hark the Heav’n of heav’ns is ring -- ing:
  Christ the Lord to man is born!
  Are not all our hearts too sing -- ing,
  Wel -- come, wel -- come, Christ -- mas morn?
  Still the Child, all pow’r pos -- sess -- ing,
  Smiles as through the a -- ges past;
  And the song of Christ -- mas bless -- ing
  Sweet -- ly sinks to rest at last.
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c {
  d4. d8 b'4 a |
  d,4. d8 g4 fis |
  b4. b8 a4 g |
  fis a a2 |
  
  d,4. d8 b'4 a |
  d,4. d8 g4 fis |
  b4. b8 b4 e |
  d cis d2 |
  
  d4. d8 d4 d |
  d b b ais |
  b b a d |
  cis b cis2 |
  
  b4 a a a |
  g a g g |
  b b a a8[ b] |
  a4. g8 fis2 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  d4. d8 d4 d |
  d4. d8 d4 d |
  b4. b8 cis4 cis |
  d fis a2 |
  
  d,4. d8 d4 d |
  d4. d8 d4 d |
  b4. b8 e4 e |
  fis fis b,2 |
  
  g4. b8 d4 d |
  b4 d fis fis |
  b gis a d, |
  e e a,2 |
  
  b4 cis d d |
  e fis g g |
  e g a fis8[ g] |
  a4 a, d2 \bar "|."
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
     \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((padding . -0.5))} \lyricsto "altos" \altoWords
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
    \tempo 4 = 105
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}
