\version "2.24.0"
\include "util.ly"
 \header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Up! Good Christen folk and listen"}}
  poet = \markup\oldStyleNum"George Ratcliffe Woodward (1848–1934)"
  composer = \markup \concat{ \italic "O quam mundum" ", from " \italic "Piæ Cantiones" \oldStyleNum", 1582"}
  arranger = \markup\oldStyleNum"Arranged by George Ratcliffe Woodward (1848–1934)"
  tagline = \markup\concat { "from " \italic "The Cowley Carol Book" \oldStyleNum", 1919"}
}
\paper {
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -5.35)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #033
  print-first-page-number = ##t
  headerLine = \markup{\override #'(font-name . "Garamond Premier Pro") \smallCapsOldStyle"christmas"}
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \if \should-print-page-number
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine
        \if \should-print-page-number
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20))) }
global = {
  \key c \major
  \time 4/4
  \autoBeamOff
}

sopMusic = \relative c' {
  c4 e g2 |
  f8 g a b g2 |
  c4 b g e |
  f8 e d4 c2\fermata \break
  \repeat volta 2 {
    g'4 g a g |
    
    c d b a |
    g b c g |
    e2( g4 a |
    g) f e2 |
    g4 g e d |
    c c g' g |
    
    e g a g |
    e( a g e) |
    d2 c |
  } \break
  c'4 c b d |
  e d b a |
  g g e a |
  
  g8[ e] f4 g2 |
  c4 c g g |
  a a e e |
  d d e f |
  e2( d4) d |
  c1\fermata \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  c4 c d( e) |
  f8 e e g g2 |
  g4 g e e |
  d8 c b4 c2 |
  e4 e e g |
  
  e f d d |
  e g g d |
  c1( |
  c4) d b2 |
  c4 c c bes |
  a c c d |
  
  c d e d |
  c2( b4 c) |
  a( b) c2 |
  g'4 a g g8[ f] |
  e4 fis g f |
  e d c f |
  
  d8[ c] a4 d2 |
  c4 e d c |
  c c b c |
  b a c c |
  c2. b4 |
  g1 \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsV
  Ding dong, ding
  Ding -- a dong -- a ding
  Ding dong, ding dong
  Ding -- a dong ding.
  
  \set stanza = #"1. "
  Up! good Chris -- ten folk, and list -- en
  How the mer -- ry church __ bells ring
  And from stee -- ple bid good peo -- ple
  Come a -- dore the new __ born King.
  
  \raiseLyrics
  Born of mo -- ther, blest o’er o -- ther,
  \markup\italic ex \markup\italic Ma -- \markup\italic rí -- \markup\italic a
  \markup\italic Vír -- \markup\italic gi -- \markup\italic ne
  In a sta -- ble "(’tis" no fa -- ble),
  \markup\italic Chris -- \markup\italic tus \markup\italic na -- \markup\italic tus \markup\italic hó -- \markup\italic di -- \markup\italic e.
}
altoWordsII = \lyricmode {
  \dropLyricsV
%\markup\italic 
  \repeat unfold 16 {\skip1}
  \set stanza = #"2. "
  Tell the sto -- ry how from glo -- ry
  God came down at Christ -- mas -- tide
  Bring -- ing glad -- ness, chas -- ing sad -- ness,
  show’r -- ing bless -- ings far __ and wide.
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
  \set ignoreMelismata = ##t
}
altoWordsIV = \lyricmode {
  \set stanza = #"4. "
  \set ignoreMelismata = ##t
}
altoWordsV = \lyricmode {
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  c4 c b( c) |
  c8 c c d e2 |
  c4 d c c |
  a8 f g4 e2 |
  c'4 c c d |
  
  c a g a8[ b] |
  c4 d c b |
  g2.( f4 |
  g) a g2 |
  e4 e g f8[ g] |
  a4 a g b |
  
  g g c g |
  g( f d c) |
  f( g) e2 |
  e'4 f8[ e] d[ c] b4 |
  c8[ b] a4 b c8[ d] |
  e4 b c c |
  
  b8[ c] d[ c] b2 |
  e,4 e g8[ f] e4 |
  f e g g |
  g f a a |
  g( a2) g4 |
  e1 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  c'4 a g( c,) |
  a'8 c a g c2 |
  e,4 g c, a' |
  d,8 d g4 c,2\fermata |
  c'4 c a b |
  
  a d, g f |
  c' g e g |
  c,( d e f |
  e) d e2 |
  c4 c c d8[ e] |
  f4 f e g |
  
  c, b a b |
  c( f, g c) |
  d( g,) c2 |
  c'4 f, g g |
  c, d g a8[ b] |
  c4 g a f |
  
  g8[ a] d,4 g,2 |
  a4 a b c |
  f, a e' c |
  g' d a f |
  c'( a f) g |
  c1\fermata \bar "|."
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
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
  \context {
    \Lyrics
    \override LyricText.font-size = #1.3
  }
    \context {
      \Score
      %\override SpacingSpanner.base-shortest-duration = #(ly:make-moment 1/8)
      %\override SpacingSpanner.common-shortest-duration = #(ly:make-moment 1/4)
    }
    \context {
      % Remove all empty staves
      % \Staff \RemoveEmptyStaves \override VerticalAxisGroup.remove-first = ##t
    }
    \context {
      \Lyrics
      \override LyricText.X-offset = #center-on-word
    }
  }
}

