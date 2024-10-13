\version "2.14.2"
\include "util.ly"
\header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"Although at Yule it Bloweth Cool"}}
    poet = \markup\oldStyleNum"George Ratcliffe Woodward (1848–1934)"
    composer = \markup \concat{\italic "Der wind der wet, der han der kret" \oldStyleNum", 1554"}
    arranger = \markup\oldStyleNum"Arranged by Charles Wood (1866–1926)"
    tagline = \markup \concat{ "from " \italic "The Cambridge Carol Book" \oldStyleNum", 1924"}
  }\paper {
  print-all-headers = ##f
  paper-height = 9\in
  paper-width = 6\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 10)
       (minimum-distance . 10)
       (padding . 1.5)
       (stretchability . 100))
  score-markup-spacing = 
    #'((basic-distance . 5)
       (minimum-distance . 5)
       (padding . 1)
       (stretchability . 0))
  ragged-last-bottom = ##t
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #078
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

\markup \fill-line{\concat{ "Words from " \italic"HymnsAndCarolsOfChristmas.com" ", Music from " \italic"CyberHymnal.org"}}
\markup\vspace#0.5


















global = {
  \key g \major
  \time 3/4
  \autoBeamOff
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(-1 . 1)
}

sopMusic = \relative c' {
  \repeat volta 2 {
    \partial 8 g'8 |
    d'4 d8 d4 g,8 |
    d'4 d8 d4 d8 |
    c4 d8 e4 e8 |
    d4. b4 \bar""\break b8 |
    
    b4 b8 b4 b8 |
    d4 d8 a4 a8 |
    a4 c8 b4 a8 |
    \partial 8*5 g4. g4
  }
  \break
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  g'8 |
  b4 g8 fis4 g8 |
  b8[ a] g fis4 g8 |
  a4 b8 c[ b] a |
  b4. g4 e8 |
  
  d4 e8 g4 e8 |
  b[ a] b d4 fis8 |
  c[ d] e d e[ fis] |
  g4. g4
}
altoWords = \lyricmode {
  \set stanza = #"1. "
  Al -- though at Yule it blow -- eth cool,
  And frost doth grip the fin -- gers,
  And nip the nose, and numb the toes,
  Of out -- door Car -- ol sing -- ers,
  
  \set stanza = #"4. "
  By tho -- rough -- fare, through slum or square,
  Our Choir the praise re -- hear -- ses
  "(As" on we pass) of ‘Wen -- ces -- las’
  That ‘Good King,’ and his mer -- cies.
}
altoWordsII = \lyricmode {
%\markup\italic
  \set stanza = #"2. "
  Through snow or sleet we pace the street,
  Fair sirs, with right good rea -- son,
  To wish you all, both great and small,
  The bless -- ings of the sea -- son.
  
  \set stanza = #"5. "
  Then we can sing, a pret -- ty thing,
  ‘The Holly and I -- vy ber -- ry;’
  But best we ken ‘Good gen -- tle -- men,
  God rest you, rest you mer -- ry.’
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
  No itch -- ing palms have we for alms,
  Con -- tent if Christ, the bur -- den
  Of these our lays, be -- stow His praise,
  And one day be our guer -- don
  
  \set stanza = #"3. "
  We think to spell, ‘Good news, No -- el,
  And eke a won -- der sto -- ry:
  The Vir -- gin mild hath borne the Child:
  E’en God, the King of Glo -- ry.’
  
}
altoWordsIV = \lyricmode {
}
altoWordsV = \lyricmode {
}
altoWordsVI = \lyricmode {
}
tenorMusic = \relative c' {
  gis8 |
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  e8 |
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
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWords
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
    \tempo 4 = 120
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}
