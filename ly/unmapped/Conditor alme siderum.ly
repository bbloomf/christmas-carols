\version "2.14.2"
\include "util.ly"
\version "2.14.2"
#(set-global-staff-size 15) \paper{ #(define fonts (make-pango-font-tree "GoudyOlSt BT" "Garamond Premier Pro" "Garamond Premier Pro" (/ 15 20))) }
\header {tagline = ""
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"Conditor alme siderum"}}
    poet = \markup\oldStyleNum"Vesper Hymn"
    composer = \markup\oldStyleNum"Arranged by Michael Praetorius (1571–1621)"
    
    tagline = \markup{from \italic"cpdl.org"}
  }
\paper {
  print-all-headers = ##f
  paper-height = 9\in
  paper-width = 6\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -10)
       (stretchability . 100))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0)
       (stretchability . 0))
  markup-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0)
       (stretchability . 0))
  ragged-last-bottom = ##t
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #008
  print-first-page-number = ##t
  headerLine = \markup{\override #'(font-name . "Garamond Premier Pro") \smallCaps advent}
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

\markup \fill-line{\concat{ "from " \italic "Peters’ Sodality Hymn Book" \oldStyleNum", 1914, via " \italic "books.google.com"}}
\markup\vspace#0




























%IF_NOT_6.14
global = {
  \key f \major
  \time 16/4
  \autoBeamOff
}

sopMusic = \relative c'' {
  \repeat volta 2 {
    \partial 4*12 a2 f4 a c c d bes c2 b\rest |
    \partial 4*12 c4 d bes c c2( bes4) a g( a) a2 |
    \partial 4*11 c4 bes g bes a bes g f2 b\rest |
    \partial 4*15 f4 a bes c c2 bes( a4) g( a) a1
  }
}
sopWords = \lyricmode {
  \repeat unfold 11 \skip1
  lux cre -- dén -- ti -- um,
  
  \repeat unfold 11 \skip1
  \dropLyricsIV
  pre -- ces súp -- pli -- cum.
}

altoMusic = \relative c' {
  \partial 4*12 e2 d4 c e f f d e2 s |
  \partial 4*12 f4 f d c8([ d e f g f d e] f4) d e8 e f2 |
  \partial 4*11 e4 d e d f f e c2 s |
  \partial 4*15 d4 e g e f2 d8[( e] f4. e8 d4) f e1 \bar ":|"
}
altoWords = \lyricmode {
  \dropLyricsVI
  \set stanza = #"1."
  Con -- dí -- tor al -- me sí -- de -- rum,
  Æ -- tér -- na lux __ cre -- dén -- ti -- um,
  \dropLyricsIX
  Chri -- ste, Re -- dém -- ptor óm -- ni -- um,
  Ex -- áu -- di pre -- ces súp -- pli -- cum.
}
altoWordsII = \lyricmode {
%\markup\italic
}
altoWordsIII = \lyricmode {
}
altoWordsIV = \lyricmode {
  
}

tenorMusic = \relative c' {
  \partial 4*12 cis2 d4 a g a a g g2 s |
  \partial 4*12 a4 bes f a g4.( a8 bes4 c) d4. cis8 d2 |
  \partial 4*11 c4 f, c' bes d d c a2 s |
  \partial 4*15 bes4 c d c a2 bes8([ c] d4 c bes) d cis1 \bar ":|"
}
tenorWords = \lyricmode {
  \repeat unfold 11 \skip1
  \dropLyricsIV
  lux cre -- dén -- ti -- um,
  
}
%  \repeat unfold 11 \skip1
%  pre -- ces súp -- pli -- cum.

bassMusic = \relative c {
  \partial 4*12 a2 d4 f c f d g c,2 d\rest |
  \partial 4*12 f4 bes, bes f' c8[( d e f] g4) a( bes a8) a d,2 |
  \partial 4*11 a4 bes c g' d bes a f2 d'\rest |
  \partial 4*15 bes4 a g c, f2 g8([ f16 e d8 e] f4 g) d a'1 \bar ":|"
}
bassWords = \lyricmode {
  \repeat unfold 11 \skip1
  lux cre -- dén -- ti -- um,
  
}
%  \repeat unfold 11 \skip1
%  pre -- ces súp -- pli -- cum.

\score {
  <<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women"  \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((padding . -0.5))} \lyricsto "altos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"men" } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" } \lyricsto "basses" \bassWords
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
      \Staff
      \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
      \remove "Time_signature_engraver"
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
