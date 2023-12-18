\version "2.24.0"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"The Son of God is born for all"}}
  subtitle = \markup{\override #'(font-name . "Garamond Premier Pro"){ \abs-fontsize #12.5 "(Geborn ist Gottes Sönelein)"}}
  poet = \markup\oldStyleNum"Michael Praetorius (1571–1621)"
  composer = \markup{"Variation of" \italic"Puer nobis nascitur" from \italic "Piæ Cantiones"}
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
       (padding . -5)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #156
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
  \time 3/4
  \autoBeamOff
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(-1 . 1)
}

sopMusic = \relative c' {
  \repeat volta 3 {
    \partial 4 ees4 |
    ees2 f4 |
    g2 aes4 |
    g2 f4 |
    ees2 \bar "" ees4 |
    bes'2 bes4 |
    
    bes4( c) d |
    ees2 ees4 |
    ees2 \bar "" \break bes4 |
    ees2 ees4 |
    d2 bes4 |
    
    c2 c4 |
    bes2 \bar "" aes4 |
    bes2 g4 |
    f2 g4 |
    ees2 d4 |
    \partial 2 ees2\fermata \break
  }
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  bes4 |
  c2 d4 |
  ees2 ees4 |
  ees2 bes4 |
  g2 c4 |
  ees( aes) g |
  
  f2 f4 |
  ees2 aes4 |
  g2 f4 |
  ees2 g4 |
  g2 g4 |
  
  ees2 ees4 |
  ees2 c4 |
  f2 ees4 |
  d2 ees4 |
  c2 bes4 |
  bes2
}
altoWords = \lyricmode {
  \dropLyricsIX
  \set stanza = #"1. "
  The Son of God is born for all
  At Beth -- lem in a cat -- tle -- stall:
  He li -- eth in a crib full small,
  And wrapt in swad -- dling -- clothes with -- al.
  
  \set stanza = #"3. "
  Be -- neath Him set His crib, of tree;
  Let Hope the lit -- tle mat -- tress be,
  His pil -- low Faith, full fair to see,
  With cov -- er -- let of Cha -- ri -- ty.
  
  \set stanza = #"5. "
  Draw nigh, the Son of God to kiss,
  Greet Ma -- ry’s Child "(the" Lord He is)
  Up -- on those love -- ly lips of His:
  Je -- sus, your hearts’ de -- sire and bliss.
  
  \set stanza = #"7. "
  \markup\italic By, \markup\italic by, \markup\italic lul -- \markup\italic lay be -- fore Him sing;
  Go, wind the horn, and pluck the string,
  Till all the place with mu -- sic ring;
  And bid one prayer to Christ the King.
  
  \set stanza = #"9. "
  Sleep, in my soul en -- shrin -- ed rest:
  Here find Thy cra -- dle neat -- ly drest:
  For -- sake me not, when sore dis -- trest,
  Em -- ma -- nu -- el, my Bro -- ther blest.
}
altoWordsII = \lyricmode {
  \dropLyricsIX
  \set stanza = #"2. "
  Re -- joice to -- day for Je -- su’s sake,
  With -- in your hearts His cra -- dle make:
  A shrine, where -- in the Babe may take
  His rest, in slum -- ber or a -- wake.
  
  
  \set stanza = #"4. "
  In bod -- ies pure and un -- de -- fil’d
  Pre -- pare a cham -- ber for the Child:
  To Him give in -- cense, myrrh and gold,
  Nor rai -- ment, meat and drink with -- hold.
  
  \set stanza = #"6. "
  Come rock His cra -- dle cheer -- i -- ly,
  As doth His moth -- er, so do ye,
  Who nurs’d Him sweet -- ly on her knee,
  As told it was by pro -- phe -- cy.
  
  \set stanza = #"8. "
  Thus, Babe, I min -- i -- ster to Thee,
  E’en as Thine An -- gels wait on me:
  Thy rud -- dy coun -- te -- nance I see,
  And ti -- ny hands out -- stretch’d to me.
  
  \set stanza = #"10. "
  Now chant we mer -- ri -- ly \markup\italic i -- \markup\italic o
  With such as play \markup\italic in \markup\italic ór -- \markup\italic ga -- \markup{\italic "no" ";"}
  And with the sing -- ers \markup\italic in \markup\italic cho -- \markup\italic ro
  \markup\italic Be -- \markup\italic ne -- \markup\italic di -- \markup\italic cá -- \markup\italic mus \markup\italic Dó -- \markup\italic mi -- \markup\italic no.
}
altoWordsIII = \lyricmode {
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
  g4 |
  g2 bes4 |
  bes2 ees,4 |
  bes'( c) d |
  ees2 ees4 |
  ees2 bes4 |
  
  bes2 bes4 |
  g2 c4 |
  bes2 bes4 |
  g2 c4 |
  bes2 d4 |
  
  c2 aes4 |
  g2 aes4 |
  f2 bes4 |
  bes2 bes4 |
  aes( g) f |
  g2
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  ees4 |
  c2 bes4 |
  ees( d) c |
  bes( aes) bes |
  ees2 aes4 |
  g( f) ees |
  
  d2 bes4 |
  c2 aes4 |
  ees'2 d4 |
  c2 ees4 |
  g2 g,4 |
  
  aes2 c4 |
  ees2 f4 |
  d2 ees4 |
  bes( aes) g |
  aes2 bes4 |
  ees,2\fermata
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
      \new Voice = "sopranos" { \voiceOne << \global \repeat unfold 5 \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \repeat unfold 5 \altoMusic >> }
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
      \new Voice = "tenors" { \voiceOne << \global \repeat unfold 5 \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \repeat unfold 5 \bassMusic >> }
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

