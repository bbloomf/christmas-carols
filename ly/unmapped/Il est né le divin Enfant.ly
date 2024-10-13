\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"Il est né le divin Enfant"}}
  poet = \markup\oldStyleNum"Anonymous"
  composer = \markup\oldStyleNum"17th century French melody"
  arranger = \markup\oldStyleNum"Arranged by Bernard Dewagtere"
  tagline = \markup { "from" \italic {www.free-scores.com}}
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
  first-page-number = #174
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
  \key a \major
  \time 4/4
  \autoBeamOff
  \set Score.voltaSpannerDuration = #(ly:make-moment 4 4)
}

sopMusic = \relative c' {
  \repeat volta 4 {
    e4 a a cis8 a |
    e4 a a2 |
    a4 a8 b cis4 d8 cis |
    b4 a b b |
    e, a a cis8 a |
    e4 a a2 |
    
    a4 b cis d8 cis |
    b4 e a,2 |
  }
  \alternative {
    {
      cis4 d e d8 cis |
      d4 fis e2 |
      cis4 d e fis8 e |
      d4 cis cis b |
      
      cis d e d8 cis |
      d4 fis e2 |
      cis4 d e fis8 e |
      d4 cis b2 |
    }
    {
      a4 b cis d8 cis |
      b2 e |
      a1\fermata \bar "|."
    }
  }
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  \repeat volta 4 {
    a4 fis' e e8 e |
    a,4 fis' e2 |
    e4 a8 gis a[ gis] fis e |
    d4 dis e e |
    a, fis' e e8 e |
    a,4 fis' e2 |
    
    e4 fis8[ gis] a4 fis8 e |
    d[ fis] e[ d] cis2 |
  }
  \alternative {
    {
      a'4 b cis b8 a |
      b4 d cis2 |
      a4 a a8[ cis] a a |
      a4 fis8[ a] a4 gis |
      
      a b cis b8 a |
      b4 d cis2 |
      a4 a a a8 a |
      a4 e8[ fis] gis2 |
    }
    {
      cis,4 gis' e e8 a |
      a2. gis4 |
      <e a>1\fermata \bar "|."
    }
  }
}
altoWords = \lyricmode {
  Il est né le di -- vin En -- fant,
  Jou -- ez haut -- bois, ré -- son -- nez mu -- set -- tes!
  Il est né le di -- vin En -- fant.
  Chan -- tons tous son a -- vè -- ne -- ment.
  \break
  \dropLyricsV
  \set stanza = #"1. "
  De -- puis plus de qua -- tre mille ans,
  Nous le pro -- met -- taient les pro -- phè -- tes,
  De -- puis plus de qua -- tre mille ans,
  Nous at -- ten -- dions cet heu -- reux temps.

}
altoWordsII = \lyricmode {
  \dropLyricsV
  \repeat unfold 34 ""
  \set stanza = #"2. "
  Une é -- tabl’ est son lo -- ge -- ment,
  Un peu de paille est sa cou -- chet -- te,
  Une é -- tabl’ est son lo -- ge -- ment,
  pour un Dieu, quel -- "(e)" dé -- nue -- ment!
  
  
  Chan -- tons tous son a -- vè -- ne -- ment.
}
altoWordsIII = \lyricmode {
  \dropLyricsV
  \repeat unfold 34 ""
  \set stanza = #"3. "
  O Jé -- sus, ô roi tout puis -- sant,
  Tout pe -- tit en -- fant que vous ê -- tes,
  O Jé -- sus, ô roi tout puis -- sant,
  Ré -- gnez sur nous en -- tiè -- re -- ment.
}
altoWordsIV = \lyricmode {
}

tenorMusic = \relative c' {
  
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  \repeat volta 4 {
    cis4 d cis a8 cis |
    cis4 d8[ b] cis2 |
    d1\rest |
    d\rest |
    cis4 d cis a8 cis |
    cis4 d8[ b] cis2 |
    
    d1\rest |
    d\rest |
  }
  \alternative {
    {
      d\rest |
      d\rest |
      a'4 b cis8[ a] d cis |
      b4 a e' e |
      
      d,1\rest |
      d\rest |
      a'4 b cis d8 cis |
      b4 a e2 |
    }
    {
      fis4 e a b8 cis |
      d2 e |
      a,1\fermata \bar "|."
    }
  }
}
bassWords = \lyricmode {

}

\score {
  <<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
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
  
}

\score {
  \unfoldRepeats

  <<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
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
  
  \midi {
    \tempo 4 = 125
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}

