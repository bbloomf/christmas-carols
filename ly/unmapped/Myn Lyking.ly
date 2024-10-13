\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"Myn Lyking"}}
  poet = \markup\oldStyleNum"15th Century"
  composer = \markup\oldStyleNum"Richard R. Terry (1865–1938)"
  tagline = \markup\concat{ "from " \italic"Twelve Christmas Carols" \oldStyleNum", 1912, via " \italic"HymnsAndCarolsOfChristmas.com"}
}
\paper {
  paper-height = 9\in
  paper-width = 6\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -5)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #186
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
#(set-global-staff-size 13.5) \paper{ #(define fonts (make-pango-font-tree "GoudyOlSt BT" "Garamond Premier Pro" "Garamond Premier Pro" (/ 13.5 20))) }
global = {
  \key g \major
  \time 4/4
  \autoBeamOff
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(-0 . 0)
  \mergeDifferentlyHeadedOn
  \mergeDifferentlyDottedOn
}
verseRests = { r1 | r1 | r1 | r2 r4 }
verseRestsII = { r1 | r1 | r1 | r2 r4^\markup\italic"Fine." }
verseMusic = \relative c'' {
  g4^\mf |
  
  a8 a d4 c8[ b] a[ g] |
  a d,4 d8 d4. d8 |
  e[ fis] g d e fis g b |
  d b e[ b] d2 \bar "||" \break
}
verseMusicII = \relative c'' {
  g4^\mf |
  
  a4 d8 d c8[ b] a[ g] |
  a4 d,8 d d4. d8 |
  e^\markup\italic"cresc." fis g d e fis g b |
  d--^\markup\italic"rall." b-- e-- b-- d2 \bar "||" 
  \once \override Score.RehearsalMark #'break-visibility = #end-of-line-visible
  \once \override Score.RehearsalMark #'self-alignment-X = #RIGHT
  \mark "D.S. al Fine"
  \break 
  
  a8^\mf a d d c8 b a g |
  a4 d,8 d d2 |
  e8 fis g d e fis g^\markup\italic"cresc. molto rall." b |
  d[-- b] e-- b-- d2 \bar "||"
  \once \override Score.RehearsalMark #'break-visibility = #end-of-line-visible
  \once \override Score.RehearsalMark #'self-alignment-X = #RIGHT
  \mark "D.S. al Fine"
  \break 
  
  a8^\mf a d4 c8[ b] a[ g] |
  a4 d,8 d d4. d8 |
  e fis g d e fis g^\markup\italic"cresc. molto rit." b |
  d b e[ b] d2 \bar "|."
  \once \override Score.RehearsalMark #'break-visibility = #end-of-line-visible
  \once \override Score.RehearsalMark #'self-alignment-X = #RIGHT
  \mark "D.S. al Fine"
}

sopMusic = \relative c'' {
  \mark \markup { \musicglyph #"scripts.segno" }
  g4^\markup\italic"a tempo" g8 a b4 d8 b |
  a4 a8 g a4 b |
  g^\markup\italic"cresc. e rall." g8 a b4 c8 b^\markup\italic"dim." |
  a^\markup\italic"rall." g a4 g2\fermata \break
}
verseWords = \lyricmode {
  \set stanza = #"1. "
  I saw a fair May -- den syt -- tin and sing.
  She lul -- lèd a lyt -- tel Childe, a swee -- té Lord -- ing.
  
  \repeat unfold 21 {\skip1}
  \set stanza = #"2. "
  That same Lord is He that made al -- lé thing,
  Of al -- lé lord -- is He is Lord, of al -- lé kyng -- es Kyng.
  
%8.5x11g  \override LyricText #'font-size = #0.8
  \set stanza = #"3."
  There was mick -- le mel -- o -- dy at that Chyld -- é’s birth.
  All that were in heav’n -- ly bliss, they made mick -- le mirth.
  
%8.5x11g  \override LyricText #'font-size = #1.3
  \set stanza = #"4. "
  An -- gels bright sang their song to that Chyld;
  Blyss -- id be Thou, and so be She, so meek and so mild.
}
sopWords = \lyricmode {
  \repeat unfold 22 {\skip1}
  Lul -- lay myn lyk -- ing, my dere sonne, my sweet -- ing.
  Lul -- lay my dere herte, myn own dere der -- ling.
}

altoMusic = \relative c' {
  b4 c b2 |
  c~ c8 e d[ c] |
  b4 b8 c d4 g8 fis |
  e g fis[ e] d2
}
altoWords = \lyricmode {
  Lul -- la -- lay.
  Lul -- la -- lay.
  Lul -- lay my dere herte, myn own dere der -- ling.
}
altoWordsII = \lyricmode {
%\markup\italic
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
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
  d,4^\pp e d2 |
  e~ e8 g fis[ e] |
  d4 g8 g g4 c8 d |
  e[ b] c4 c b\fermata
}
tenorWords = \lyricmode {
  Lul -- la -- lay.
  Lul -- la -- lay.
  Lul -- lay my dere herte, myn own dere der -- ling.
}

bassMusic = \relative c {
  g4 g g2 |
  g4 g g2 |
  g2 g'8[ fis] e[ d] |
  c e d4 g,2\fermata
}
bassWords = \lyricmode {
  Lul -- la -- lay.
  Lul -- la -- lay.
  Lul -- lay, myn own dere der -- ling.
}

chorusRests = { s1*8 }

pianoRH = \relative c' {
  << \new Voice { \voiceOne 
                  d'4\( g8[ d] e2 |
                  d4 a g e'~ |
                  e8[ d b a]~ a[ g a b] |
                  a[ g] <g e>4 <g d>2\) |
                  
                  a4 d c8[ b a g] |
                  a[ d d c]~ c[ b a g] |
                  e[ fis] g4 e8[ fis] g4 |
                  d' e8[ b] <d fis,>2 |
                  
                  g,4. a8 b2 |
                  a4. g8 a4 b |
                  g4. a8 b4 c8[ b] |
                  a[ g] a4 g2\fermata |
                  
                  
                  d'4\( g8[ d] e2 |
                  d4 a g e'~ |
                  e8[ d b a]~ a[ g a b] |
                  a[ g] <g e>4 <g d>2\) |
                  
                  a4 d c8[ b a g] |
                  a[ d~ d c]~ c[ b a g] |
                  e[ fis] g4 e8[ fis] g4 |
                  d' e8[ b] <d fis,>2 |
                  
                  a4 d c8[ b a g] |
                  a[ d~ d c]~ c[ b a g] |
                  e[ fis] g4 e8[ fis] g4 |
                  d' e8[ b] <d fis,>2 |
                  
                  a4 d c8[ b a g] |
                  a[ d~ d c]~ c[ b a g] |
                  e[ fis] g4 e8[ fis] g4 |
                  d' e8[ b] <d fis,>2 |
                }
     \new Voice { \voiceTwo
                  g,2 c8[ b a g]~ |
                  g4 fis g c8[ b] |
                  a4 fis e d |
                  <c e> g8[ a] b2 |
                  
                  d4 fis g d |
                  g <a fis> g d |
                  c d c b |
                  fis' g d c |
                  
                  b c b2 |
                  c~ c8[ e d c] |
                  b4. c8 d4 g8[ fis] |
                  e[ g] fis[ e] d2 |
                  
                  
                  g2 c8[ b a g]~ |
                  g4 fis g c8[ b] |
                  a4 fis e d |
                  <c e> g8[ a] << b2 { s16 s4..^\markup\italic"Fine."} >> |
                  
                  d4 fis g d |
                  g <a fis> g d |
                  c d c b |
                  fis' g d c |
                  
                  d4 fis g d |
                  g <a fis> g d |
                  c d c b |
                  fis' g d c |
                  
                  d4 fis g d |
                  g <a fis> g d |
                  c d c b |
                  fis' g d c |
                }
  >>
}
pianoLH = \relative c' {
  << \new Voice { \voiceOne 
                  b4^\f g2 c4 |
                  a d8[ a] c[ b a g] |
                  a4 c b s |
                  s1 |
                  
                  s1^\mf |
                  s1 |
                  s1 |
                  b4. e8 s4 d,4 |
                  
                  d4^\markup{\dynamic"pp" \italic "a tempo"} e d2 |
                  e2~ e8[ g fis e] |
                  d4^\markup\italic"rall."^\< g~ g c8[\! d] |
                  e[ b]^\> c4~ c\! b\fermata |
                  
                  
                  b4^\markup{\dynamic"mf" \italic"a tempo"} g4 g c |
                  a d8[ a] c[ b a g] |
                  a4 c b s |
                  s2. s4 |
                  
                  s1^\mf |
                  s1 |
                  s1^\markup\italic"cresc." |
                  b4.^\markup\italic"rall." e8 s2 |
                  
                  s1^\mf |
                  s1 |
                  s2 s8 s4.^\markup\italic"cresc. molto rall." |
                  b4. e8 s2 |
                  
                  s1^\mf |
                  s1 |
                  s2 s4 s^\markup\italic"cresc. molto rit." |
                  b4. e8 s2 |
                }
     \new Voice { \voiceTwo
                  g,,4\( b c2 |
                  d e |
                  fis4 d e b |
                  c c, g'2\) |
                  
                  fis'4 d g b, |
                  d d, g  b |
                  c b a g |
                  b e b d, |
                  
                  g1~ |
                  g~ |
                  g2 g'8[ fis e d] |
                  c[ e] d4 g,2\fermata
                  
                  
                  g4\( b c2 |
                  d e |
                  fis4 d e b |
                  c c, g'2\) |
                  
                  fis'4 d g b, |
                  d d, g  b |
                  c b a g |
                  b e b <d d,> |
                  
                  fis4 d g b, |
                  d d, g  b |
                  c b a g |
                  b e b <d d,> |
                  
                  fis4 d g b, |
                  d d, g  b |
                  c b a g |
                  b e b <d d,> |
                }
  >>
}

\score {
  <<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \global \tempo "Allegro moderato" 4 = 112 \verseRests \verseMusic \voiceOne \sopMusic \oneVoice \verseRestsII \verseMusicII }
      \new Voice = "altos" { \voiceTwo \global \chorusRests \altoMusic }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women"} \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women"} \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women"} \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women"} \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women"} \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women"} \lyricsto "altos" \altoWords
    \context Lyrics = "altos" \lyricsto "sopranos" \verseWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne \global \chorusRests \tenorMusic }
      \new Voice = "basses" { \voiceTwo \global \chorusRests \bassMusic }
    >>
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWords
  >>
    \new PianoStaff << \new Staff { \global \new Voice { \pianoRH } } \new Staff { \global \clef "bass" \pianoLH } >>
  >>
  \layout {
    \context {
      \Score
      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 2)
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 2)
    }
    \context {
      % Remove all empty staves
      \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
    }
    \context {
      \Lyrics
      \override LyricText #'X-offset = #center-on-word
    }
  }
  \midi {
    \tempo 4 = 90
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}
