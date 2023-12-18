\version "2.24.0"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"O Come, Little Children"}}
  poet = \markup\concat{\italic"Ihr Kinderlein kommet" ", by Christoph von Schmid (1768–1854)"}
  composer = \markup\oldStyleNum"Johann A. P. Schulz (1747–1800)"
  tagline = \markup{from \italic"HymnsAndCarolsOfChristmas.com"}
}
\paper {
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
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
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #094
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
  \key ees \major
  \time 2/4
  \autoBeamOff
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(-0.5 . 1)
}

sopMusic = \relative c'' {
  \partial 8 bes8 |
  bes4 g8 bes8\noBeam |
  bes4 g8 bes\noBeam |
  aes4 f8\noBeam f |
  g4 bes8\rest \bar""\break bes |
  
  bes4 g8 bes8\noBeam |
  bes4 g8 bes\noBeam |
  aes4 f8\noBeam f |
  g4 bes8\rest \bar""\break g |
  
  f4 f8\noBeam f |
  aes4 aes8\noBeam aes |
  g4 g8\noBeam g |
  c4. \bar""\break c8 |
  
  bes4 bes8\noBeam bes |
  ees4 bes8\noBeam g |
  aes4 f8\noBeam f |
  \partial 4. ees4 bes'8\rest \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c'' {
  g8 |
  g4 ees8\noBeam g |
  g4 ees8\noBeam des |
  c[ ees] ees\noBeam d |
  ees4 s8 g8 |
  
  g4 ees8\noBeam g |
  g4 ees8\noBeam des |
  c[ ees] ees\noBeam d |
  ees4 s8 ees |
  
  ees4 d8\noBeam d |
  c4 c8\noBeam d |
  ees4 ees8\noBeam ees |
  ees4. ees8 |
  
  f[ d] ees\noBeam f |
  ees4 ees8\noBeam ees |
  f[ ees] ees\noBeam d |
  bes4 s8 \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsIX
  \set stanza = #"1. "
  O come, lit -- tle chil -- dren, O come one and all,
  To Beth -- le -- hem haste, to the man -- ger so small,
  God’s Son for a gift has been sent you this night
  To be your Re -- deem -- er, your joy and de -- light.
}
altoWordsII = \lyricmode {
  \dropLyricsIX
%\markup\italic
  \set stanza = #"2. "
  He’s born in a sta -- ble for you and for me,
  Draw near by the bright gleam -- ing Star light to see,
  In swad -- dling clothes ly -- ing so meek and so mild,
  And pur -- er than an -- gels the heav -- en -- ly Child.
}
altoWordsIII = \lyricmode {
  \dropLyricsIX
  \set stanza = #"3. "
  See Ma -- ry and Jo -- seph with love beam -- ing eyes
  Are gaz -- ing up -- on the rude bed where He lies,
  The shep -- herds are kneel -- ing, with hearts full of love,
  While an -- gels sing loud al -- le -- lu -- ias a -- bove.
}
altoWordsIV = \lyricmode {
  \dropLyricsIX
  \set stanza = #"4. "
  Kneel down and a -- dore Him with shep -- herds to -- day,
  Lift up lit -- tle hands now and praise Him as they;
  Re -- joice that a Sav -- ior from sin you can boast,
  And join in the song of the heav -- en -- ly host.
}
altoWordsV = \lyricmode {
}
altoWordsVI = \lyricmode {
}
tenorMusic = \relative c' {
  bes8 |
  bes4 bes8\noBeam bes |
  bes[ ees] bes\noBeam g |
  aes4 bes8\noBeam bes |
  bes4 s8 bes8 |
  
  bes4 bes8\noBeam bes |
  bes[ ees] bes\noBeam g |
  aes4 bes8\noBeam bes |
  bes4 s8 bes8 |
  
  bes4 bes8\noBeam bes |
  aes4 c8\noBeam bes |
  bes4 c8\noBeam des |
  c4. c8 |
  
  d[ bes] c\noBeam d |
  bes[ aes] g\noBeam c |
  c[ aes] bes\noBeam aes |
  g4 s8 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  ees8 |
  ees4 ees8\noBeam ees |
  ees4 ees8\noBeam e |
  f4 bes,8\noBeam bes |
  ees4 d8\rest ees8 |
  
  ees4 ees8\noBeam ees |
  ees4 ees8\noBeam e |
  f4 bes,8\noBeam bes |
  ees4 d8\rest ees8 |
  
  bes4 bes8\noBeam bes |
  f'4 f8\noBeam bes, |
  ees4 c8\noBeam bes |
  aes4. aes'8 |
  
  aes4 aes8\noBeam aes |
  g[ f] ees\noBeam c |
  f4 bes,8 bes |
  ees4 d8\rest \bar "|."
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
%    \new Lyrics = sopranos \with { \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) }
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . -0.5)) } \lyricsto "sopranos" \altoWords
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

