\version "2.24.0"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Puer natus in Bethlehem"}}
  subtitle = \markup{\override #'(font-name . "Garamond Premier Pro"){ \abs-fontsize #12.5 "(A Babe is Born in Bethlehem)"}}
  poet = \markup\oldStyleNum"14th century or earlier"
  composer = \markup \concat{"From " \italic "Piæ Cantiones" \oldStyleNum"*, 1582"}
  tagline = \markup { \center-column { \concat { "from " \italic "The Cowley Carol Book" \oldStyleNum", 1919"} \justify {*In \italic Piæ \italic Cantiones only a tenor and bass part were given, and in \italic The \italic Cowley \italic Carol \italic Book (and here), the bass line from \italic Piæ \italic Cantiones is found in the soprano, while the tenor is retained as the tenor.}}}
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
       (padding . -5)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #154
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
  \key f \major
  \time 3/4
  \autoBeamOff
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(-1 . 1)
}

sopMusic = \relative c' {
  \repeat volta 4 {
    \partial 4 g'4 |
    g2 g4 |
    a2 a4 |
    bes2 g4 |
    \partial 2 f2 \bar "||"
    \partial 4 f4 |
    bes2( c4 |
    
    bes2) a4 |
    \partial 2 bes2\fermata \bar "||" \break
    \partial 4 bes4 |
    bes2 bes4 |
    a2 g4 |
    g2 f4 |
    \partial 2 g2 \bar "||"
    
    \partial 4 g4 |
    f2( g4 |
    a2 f4 |
    bes2 a4 |
    g2) f4 |
    \partial 2 g2\fermata
  }
  \break
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  g'4 |
  bes,2 d4 |
  f2 f4 |
  f2 ees4 |
  d2 \bar "||"
  d4 |
  f2( f4 |
  
  f2) f4 |
  d2 \bar "||"
  d4 |
  g2 f4 |
  f2 d4 |
  ees2 d4 |
  bes2 \bar "||"
  
  d4 |
  d2.( |
  f |
  f |
  d2) d4 |
  d2
}
altoWords = \lyricmode {
  \dropLyricsV
  \set stanza = #"1. "
  Pu -- er na -- tus in Béth -- le -- hem,
  Al -- le -- lú -- ia.
  Un -- de gau -- det Je -- rú -- sa -- lem,
  Al -- le -- lú -- ia.
  
  \set stanza = #"5. "
  Si -- ne ser -- pén -- tis vúl -- ne -- re,
  Al -- le -- lú -- ia.
  De nos -- tro ve -- nit sán -- gui -- ne,
  Al -- le -- lú -- ia.
  
  \set stanza = #"9. "
  Co -- gnó -- vit bos et á -- si -- nus
  Al -- le -- lú -- ia.
  Quod Pu -- er e -- rat Dó -- mi -- nus.
  Al -- le -- lú -- ia.
  
  \set stanza = #"12. "
  In -- trán -- tes do -- mum ín -- vi -- cem,
  Al -- le -- lú -- ia.
  Na -- tum sa -- lú -- tant Hó -- mi -- nem,
  Al -- le -- lú -- ia.
}
altoWordsII = \lyricmode {
  \dropLyricsV
%\markup\italic
  \set stanza = #"2. "
  As -- súmp -- sit car -- nem hó -- mi -- nis,
  Al -- le -- lú -- ia.
  Ver -- bum Pa -- tris al -- tís -- si -- mi,
  Al -- le -- lú -- ia.
  
  
  \set stanza = #"6. "
  In car -- ne no -- bis sí -- mi -- lis,
  Al -- le -- lú -- ia.
  Pec -- cá -- to sed dis -- sí -- mi -- lis,
  Al -- le -- lú -- ia.
  
  \set stanza = #"10. "
  Et án -- ge -- lus pas -- tó -- ri -- bus,
  Al -- le -- lú -- ia.
  Re -- vé -- lat Quis sit Dó -- mi -- nus,
  Al -- le -- lú -- ia.
  
  
  \set stanza = #"13. "
  In hoc na -- tá -- li gáu -- di -- o,
  Al -- le -- lú -- ia.
  Be -- ne -- di -- cá -- mus Dó -- mi -- no,
  Al -- le -- lú -- ia.
}
altoWordsIII = \lyricmode {
  \dropLyricsV
  \set stanza = #"3. "
  Per Ga -- bri -- é -- lis nún -- ti -- um,
  Al -- le -- lú -- ia.
  Vir -- go con -- cé -- pit Fí -- li -- um,
  Al -- le -- lú -- ia.
  
  \set stanza = #"7. "
  Tam -- quam spon -- sus de thá -- la -- mo,
  Al -- le -- lú -- ia.
  Pro -- cés -- sit ma -- tris ú -- te -- ro,
  Al -- le -- lú -- ia.
  
  \set stanza = #"11. "
  Ma -- gi de lon -- ge vé -- ni -- unt,
  Al -- le -- lú -- ia.
  Au -- rum, thus, myr -- rham óf -- fe -- runt.
  Al -- le -- lú -- ia.
  
  \set stanza = #"14. "
  Lau -- dé -- tur sanc -- ta Trí -- ni -- tas,
  Al -- le -- lú -- ia.
  De -- o di -- cá -- mus grá -- ti -- as,
  Al -- le -- lú -- ia.
}
altoWordsIV = \lyricmode {
  \dropLyricsV
  \set stanza = #"4. "
  De ma -- tre na -- tus vír -- gi -- ne,
  Al -- le -- lú -- ia.
  Si -- ne vi -- rí -- li sé -- mi -- ne,
  Al -- le -- lú -- ia.
  
  \set stanza = #"8. "
  Hic ja -- cet in præ -- sé -- pi -- o,
  Al -- le -- lú -- ia.
  Qui re -- gnat si -- ne tér -- mi -- no.
  Al -- le -- lú -- ia.
  
  
}
altoWordsV = \lyricmode {
}
altoWordsVI = \lyricmode {
}
tenorMusic = \relative c' {
  g4 |
  g2 g4 |
  c2 c4 |
  bes2 bes4 |
  bes2 \bar "||"
  bes4 |
  d2( f4 |
  
  d2) c4 |
  bes2 \bar "||"
  bes4 |
  d2 d4 |
  c2 bes4 |
  c2 a4 |
  g2 \bar "||"
  
  g4 |
  d'2.( |
  c |
  d2 c4 |
  bes2) a4 |
  g2
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  g'4 |
  g2 g4 |
  f2 f4 |
  d2 ees4 |
  bes'2 \bar "||"
  bes4 |
  bes2( a4 |
  
  bes2) f4 |
  bes,2 \bar "||"
  bes'4 |
  g2 d4 |
  f2 g4 |
  c,2 d4 |
  g,2 \bar "||"
  
  g'4 |
  bes2.( |
  f d2 f4 |
  g2) d4 |
  g,2\fermata
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
      \new Voice = "sopranos" { \voiceOne << \global \repeat unfold4\sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \repeat unfold4\altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "altos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \repeat unfold4\tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \repeat unfold4\bassMusic >> }
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

