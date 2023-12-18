\version "2.24.0"
\include "util.ly"
\paper {
  print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -5)
       (stretchability . 100))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 1)
       (stretchability . 0))
  last-bottom-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -0.5)
       (stretchability . 0))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #089
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
  \key bes \major
  \time 4/4
  \autoBeamOff
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(-1 . 1)
}

sopMusic = \relative c'' {
  \tempo \markup\italic"Not slow."
  g4 f8\noBeam g a4 a |
  \slurDashed bes bes8\noBeam( bes) a2 |
  
  g4 f8\noBeam g a4 g |
  f e d2 |
  
  g4 f8\noBeam g a4 a |
  bes bes a2 |
  
  d4 d8\noBeam c d4 c |
  bes a g2 \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  d4 d8\noBeam d f4 f |
  \slurDashed f f8\noBeam( f) f2 |
  
  d4 d8\noBeam d f4 d |
  d cis d2 |
  
  d4 d8\noBeam d f4 f |
  f f f2 |
  
  f4 f8\noBeam f f4 \slurSolid ees8[ g] |
  g4 fis d2 \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsIX
  \set stanza = #"1. "
  \set ignoreMelismata = ##t
  Whence comes this rush of wings a -- _ far,
  Fol -- low -- ing straight the No -- ël star?
  Birds from the woods in won -- drous flight,
  Beth -- le -- hem seek this _ Ho -- ly Night.
}
altoWordsII = \lyricmode {
  \dropLyricsIX
%\markup\italic
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  “Tell us, ye birds, why come ye __ _ here,
  In -- to this sta -- ble, poor and drear?”
  “Hast -- ’ning we seek the new -- born King,
  And all our sweet -- est _ mu -- sic bring.”
}
altoWordsIII = \lyricmode {
  \dropLyricsIX
  \set stanza = #"3. "
  Hark how the Green -- finch bears his __ part,
  Phi -- lo -- mel, too, with ten -- der heart,
  Chants from her leaf -- y dark re -- treat
  \markup\italic Re, \markup\italic mi, \markup\italic fa, \markup\italic sol, in ac -- cents sweet.
}
altoWordsIV = \lyricmode {
  \dropLyricsIX
  \set stanza = #"4. "
  \set ignoreMelismata = ##t
  An -- gels and shep -- herds, birds of the sky,
  Come where the Son of God doth lie;
  Christ on the earth with man doth dwell,
  Join in the shout, “No -- _ ël, No -- ël!”
}
altoWordsV = \lyricmode {
}
altoWordsVI = \lyricmode {
}
tenorMusic = \relative c' {
  bes4 a8\noBeam bes c4 c |
  \slurDashed d d8\noBeam( d) c2 |
  
  bes4 bes8\noBeam bes c4 bes |
  a \slurSolid a8[ g] f2 |
  
  \slurDashed bes4 a8\noBeam bes c4 c |
  d d c2 |
  
  bes4 bes8\noBeam a bes4 ees |
  d \slurSolid d8[ c] bes2 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c' {
  g4 d8\noBeam g f4 f |
  \slurDashed bes bes8\noBeam( bes) f2 |
  
  g4 bes8\noBeam g f4 g |
  a a, d2 |
  
  g4 d8\noBeam g f4 f |
  bes bes f2 |
  
  bes4 bes8\noBeam f bes4 c |
  d d, g2 \bar "|."
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
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . -0.5))} \lyricsto "altos" \altoWords
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
%6x9  \context {\Lyrics\override LyricText.font-size = #0.75 }
%6.14 \context {\Lyrics\override LyricText.font-size = #0.25 }
\context {\Lyrics\override LyricText.font-size = #1.1 }
%{IF_LESSER
\context {\Lyrics\override LyricText.font-size = #0.6 }
%}%END_IF_LESSER
    #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20)))
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
  \header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Carol of the Birds"}}
    poet = \markup\oldStyleNum"Traditional"
    composer = \markup\oldStyleNum"Bas-Quercey Carol"
    tagline = \markup \concat{ "from " \italic"Carols Old and Carols New" \oldStyleNum", 1916, via " \italic"HymnsAndCarolsOfChristmas.com" }
  }
}
\markup \fill-line { \concat{ "from " \italic"Carols Old and Carols New" \oldStyleNum", 1916, via " \italic"HymnsAndCarolsOfChristmas.com" }}
\markup\vspace#1.25




















global = {
  \key ees \major
  \time 4/4
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(-1 . 1)
}

sopMusic = \relative c' {
  \partial 4 ees4 |
  g4. fis8 g4 g4 |
  aes4. g8 aes4 a |
  bes4 ees d c |
  c4. bes8 bes4 \bar"" \break
  
  bes4 |
  bes4. aes8 g4 aes |
  g4. f8 ees4 f |
  g aes bes c |
  d,4. f8 ees4 \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  ees4 |
  ees4. ees8 ees4 des |
  d?4. cis8 d4 ees |
  ees g g ees |
  f4. f8 f4
  
  ees4 |
  ees4. ees8 ees4 c |
  b4. b8 c4 ees |
  ees ees ees ees |
  d4. d8 ees4 \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsIX
  \set stanza = #"1. "
  I heard the bells on Christ -- mas Day Their old fa -- mil -- iar car -- ols play,
  And wild and sweet the words re -- peat Of peace on earth, good will to men.
}
altoWordsII = \lyricmode {
  \dropLyricsIX
%\markup\italic
  \set stanza = #"2. "
  I thought how, as the day had come, The bel -- fries of all Chris -- ten -- dom
  Had rolled a -- long th’un -- bro -- ken song Of peace on earth, good will to men.
}
altoWordsIII = \lyricmode {
  \dropLyricsIX
  \set stanza = #"3. "
  And in de -- spair I bowed my head, “There is no peace on earth,” I said,
For hate is strong, and mocks the song Of peace on earth, good will to men.
}
altoWordsIV = \lyricmode {
  \dropLyricsIX
  \set stanza = #"4. "
  Then pealed the bells more loud and deep: “God is not dead, nor doth He sleep;
  The wrong shall fail, the right pre -- vail, With peace on earth, good will to men.
}
altoWordsV = \lyricmode {
  \dropLyricsIX
  \set stanza = #"5. "
  Till, ring -- ing, sing -- ing on its way, The world re -- volved from night to day,
  A voice, a chime, a chant sub -- lime, Of peace on earth, good will to men.
}
altoWordsVI = \lyricmode {
}
tenorMusic = \relative c' {
  g4 |
  bes4. a8 bes4 bes |
  bes4. bes8 bes4 c |
  bes4 bes bes bes |
  d4. d8 d4
  
  ees4 |
  c4. c8 bes4 ees, |
  g4. g8 g4 a |
  bes c bes f |
  f4. aes8 g4 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  ees4 |
  ees4. ees8 ees4 e |
  f4. e8 f4 fis |
  g ees ees g |
  aes4. aes8 aes4
  
  g4 |
  aes4. aes8 ees4 ees |
  d4. d8 c4 c |
  bes aes g aes |
  bes4. bes8 ees4 \bar "|."
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
  \header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"I Heard the Bells on Christmas Day"}}
    poet = \markup\oldStyleNum"Henry Wadsworth Longfellow (1807–1882)"
    composer = \markup\oldStyleNum"John Baptiste Calkin (1827–1905)"
    tagline = \markup\concat{"from "\italic"HymnsAndCarolsOfChristmas.com"}
  }
}
\header {tagline = \markup\concat{"from "\italic"HymnsAndCarolsOfChristmas.com"}}

