\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"The Story of the Shepherd"}}
  poet = \markup\concat{\italic"Gongora" ", a Spanish Carol"}
  meter = \markup\oldStyleNum"Translated by Archdeacon Churton"
  composer = \markup\oldStyleNum"Joseph Barnby (1838–1896)"
  tagline = \markup { "from" \italic {Christmas Carols, New and Old}}
}
\paper {
  %print-all-headers = ##f
  paper-height = 9\in
  paper-width = 6\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -2)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #168
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
#(set-global-staff-size 14.9) \paper{ #(define fonts (make-pango-font-tree "GoudyOlSt BT" "Garamond Premier Pro" "Garamond Premier Pro" (/ 14.9 20))) }
global = {
  \key f \major
  \time 4/4
  \autoBeamOff
}

sopMusic = \relative c' {
  \partial 4 a'4 |
  a a f g |
  a4. bes8 a4 a |
  g f g gis |
  a2. \bar"" a4 |
  
  a a f g |
  a4. bes8 a4 d |
  c f, g bes |
  a2. \bar "|" \key d\major a4 |
  d4. d8 cis4 a |
  
  d4. d8 cis4 a |
  d cis b cis |
  a2. \bar"" a4 |
  d4. d8 cis4 a |
  d4. d8 cis4 b |
  
  e4 d b cis |
  a2. \bar"" a4 |
  b e a, a |
  g4. a8 fis4 fis |
  g g a a |
  
  b2. \bar"||" d4\segno |
  cis4. b8 a4 d |
  cis4. b8 a4 b |
  g fis fis e |
  
  a2. \bar"" d4 |
  cis4. b8 a4 d |
  cis b a b |
  a g8[ fis] e4. e8 |
  d1 \bar ":|" \break
  \once \override Score.RehearsalMark #'break-visibility = #end-of-line-visible
  \once \override Score.RehearsalMark #'self-alignment-X = #RIGHT
  \mark "Fine."
  
  
  
  
  \partial 4 a'4 |
  a a f g |
  a4. bes8 a4 a |
  g f g gis |
  a2. \bar"" a4 |
  
  a a f g |
  a4. bes8 a4 d |
  c f, g bes |
  a2. \bar "|"\break \key d\major a4 |
  d4. d8 cis4 a |
  
  d4. d8 cis4 a |
  d cis b cis |
  a2. \bar"" a4 |
  d4. d8 cis4 a |
  d4. d8 cis4 b |
  
  e4 d b cis |
  a2. a4 |
  b e a, a |
  g4. a8 fis4 fis |
  g g a a |
  
  b2. \bar"||"
  \once \override Score.RehearsalMark #'break-visibility = #end-of-line-visible
  \once \override Score.RehearsalMark #'self-alignment-X = #RIGHT
  \mark "D.S. al Fine"
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  a'4 |
  a a f g |
  a4. bes8 a4 cis, |
  d d d d |
  d2( cis4) a' |
  
  a a f g |
  a4. bes8 a4 d |
  c f, g bes |
  a2. g4 |
  fis4. fis8 e4 g |
  
  fis4. fis8 e4 g |
  fis fis gis gis |
  a( g fis) e |
  a4. a8 a4 e |
  a4. a8 a4 a |
  
  a a a gis |
  a2. a4 |
  g! g g fis |
  fis e d d |
  d d e fis |
  
  g2. g4 |
  g4. g8 fis4 e |
  e g fis fis |
  e d d cis |
  
  d2. d4 |
  e e e d8[ e] |
  fis[ a,] b[ cis] d4 d |
  fis d d cis |
  d1 |
  
  
  
  
  a'4 |
  a a f g |
  a4. bes8 a4 cis, |
  d d d d |
  d2( cis4) a' |
  
  a a f g |
  a4. bes8 a4 d |
  c f, g bes |
  a2. g4 |
  fis4. fis8 e4 g |
  
  fis4. fis8 e4 g |
  fis fis gis gis |
  a( g fis) e |
  a4. a8 a4 e |
  a4. a8 a4 a |
  
  a a a gis |
  a2. a4 |
  g! g g fis |
  fis e d d |
  d d e fis |
  
  g2.
}
dropLyrics = {
  \dropLyricsXI
}
altoWords = \lyricmode {
  \dropLyrics
  \set stanza = #"1. "
  It was the ve -- ry noon of night: the stars a -- bove the fold,
  More sure than clock or chim -- ing bell, the hour of mid -- night told:
  When from the heav’ns there came a voice, and forms were seen \set associatedVoice = "basses" to shine, __
  \unset associatedVoice
  Still bright -- ’ning as the mu -- sic rose with light and love di -- vine.
  With love di -- vine the song be -- gan; there shone a light se -- rene:
  
  
  
  \repeat unfold 28 \skip1
  \set stanza = #"4. "
  When once the rapt -- urous trance was past, that so my sense could bind,
  I left my sheep to Him whose care breathed in the west -- ern wind;
  I left them, for in -- stead of snow, I trod on blade \set associatedVoice = "basses" and flow’r, __
  \unset associatedVoice
  And ice dis -- solved in star -- ry rays at morn -- ing’s gra -- cious hour,
  Re -- veal -- ing where on earth the steps of Love Di -- vine had been;
}
altoWordsII = \lyricmode {
  \dropLyrics
%\markup\italic
  \set stanza = #"2. "
  O ne’er could night -- in -- gale at dawn sa -- lute the ris -- ing day
  With sweet -- ness like that bird of song in his im -- mor -- tal lay:
  O ne’er were wood -- notes heard at eve by banks with pop -- \set associatedVoice = "basses" lar shade __
  \unset associatedVoice
  So thrill -- ing as the con -- cert sweet by heav’n -- ly harp -- ings made;
  
  For love di -- vine was in each chord, and fill’d each pause be -- tween:  
  
  O, who hath heard what I have heard, or seen what I have seen?
  O, who hath heard what I have heard, or seen what I have seen?
  
  \set stanza = #"5. "
  I hast -- en’d to a low -- roofed shed, for so the An -- gel bade;
  And bowed be -- fore the low -- ly rack where Love Di -- vine was laid:
  A new -- born Babe, like ten -- der Lamb, with Li -- on’s strength \set associatedVoice = "basses" there smiled; __
  \unset associatedVoice
  For Li -- on’s strength, im -- mort -- al might, was in that new -- born Child;
  That Love Di -- vine in child -- like form had God for -- ev -- er been:
}
altoWordsIII = \lyricmode {
  \dropLyrics
  \set stanza = #"3. "
  I roused me at the pier -- cing strain, but shrunk as from the ray
  Of sum -- mer light -- ning; all a -- round so bright the splen -- dor lay.
  For oh, it mas -- tered sight and sense, to see that glo -- \set associatedVoice = "basses" ry shine, __
  \unset associatedVoice
  To hear that min -- strel in the clouds, who sang of Love Di -- vine,
  To see that form with bird -- like wings, of more than mor -- tal mien:
  
  
}
altoWordsIV = \lyricmode {
}
altoWordsV = \lyricmode {
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  a4 |
  a a f g |
  a4. bes8 a4 a |
  bes a g f |
  f2( e4) a |
  
  a a f g |
  a4. bes8 a4 d |
  c f, g bes |
  a2. \key d\major cis4 |
  a4. a8 a4 a |
  
  a4. a8 a4 a |
  fis' e d e |
  cis( d2) cis4 |
  d4. d8 e4 cis |
  d4. d8 e4 fis |
  
  g fis d e |
  cis2. cis4 |
  d b cis d |
  b cis d a |
  d d c c |
  
  b2. b4 |
  cis4 e a, b |
  cis e a, fis |
  b a8[ b] a4 a |
  
  a2. a4 |
  a gis a fis8[ g] |
  a[ cis] d[ e] fis4 e |
  a, b8[ a] g4 g |
  fis1 |
  
  
  
  
  a4 |
  a a f g |
  a4. bes8 a4 a |
  bes a g f |
  f2( e4) a |
  
  a a f g |
  a4. bes8 a4 d |
  c f, g bes |
  a2. \key d\major cis4 |
  a4. a8 a4 a |
  
  a4. a8 a4 a |
  fis' e d e |
  cis( d2) cis4 |
  d4. d8 e4 cis |
  d4. d8 e4 fis |
  
  g fis d e |
  cis2. cis4 |
  d b cis d |
  b cis d a |
  d d c c |
  
  b2.
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  a'4 |
  a a f g |
  a4. bes8 a4 a, |
  d4. c8 bes4 bes |
  a2. a'4 |
  
  a a f g |
  a4. bes8 a4 d |
  c f, g bes |
  a2. \key d\major a4 |
  d,4. d8 d4 d |
  
  d4. d8 d4 d |
  d d e e |
  a( b a) g |
  fis4. fis8 e4 g |
  fis4. fis8 e4 d |
  
  cis4 d fis e |
  a2 a, |
  a4 a a a |
  a a b c |
  b b a a |
  
  g2. g4 |
  a cis d gis, |
  a cis d dis |
  e fis8[ g] a4 g |
  
  fis2. f4 |
  e d cis b |
  a g fis g |
  a a a a |
  d1 |
  
  
  
  
  a'4 |
  a a f g |
  a4. bes8 a4 a, |
  d4. c8 bes4 bes |
  a2. a'4 |
  
  a a f g |
  a4. bes8 a4 d |
  c f, g bes |
  a2. \key d\major a4 |
  d,4. d8 d4 d |
  
  d4. d8 d4 d |
  d d e e |
  a( b a) g |
  fis4. fis8 e4 g |
  fis4. fis8 e4 d |
  
  cis4 d fis e |
  a2 a, |
  a4 a a a |
  a a b c |
  b b a a |
  
  g2.
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
    \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
     \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "tenors" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "tenors" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "tenors" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "tenors" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "tenors" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((padding . -0.9))} \lyricsto "tenors" \altoWords
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
