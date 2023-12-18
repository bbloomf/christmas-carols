\version "2.24.0"
\include "util.ly"
\paper {
  print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
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
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #078
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
}

sopMusic = \relative c' {
  \repeat volta 3 {
    f4 f g |
    a2 a4 |
    \slurDashed g( g) e |
    f2 c'4 |
    c( c) c |
    
    d2 d8\noBeam( d) |
    c2 c4 |
    a2 \bar""\break a8\noBeam( a) |
    a4( a) g |
    bes4( bes) a |
    
    g( g) f |
    a2. |
    c4 c bes |
    a2 f4 |
    g( g) e |
    f2.
  }
  \break
  
  \repeat volta 4 {
    f4 f g |
    a2 a8( a) |
    \slurDashed g4( g) e |
    f2 c'8( c) |
    c4( c) c |
    
    d4( d) d8\noBeam( d) |
    c2 c4 |
    a2 \bar""\break a8\noBeam( a) |
    a4( a) g |
    bes4( bes) a |
    
    g2 f4 |
    a2. |
    c4 c bes |
    a2 f4 |
    g( g) e |
    f2.
  }
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  \slurDashed
  c4 d e |
  f2 c4 |
  d4( e) c |
  c2 e4 |
  
  f( f) f |
  f2 f8( f) |
  f2 e4 |
  f2 f8( f) |
  
  f4( f) c4 |
  f4( f) f4 |
  d4( e) d4 |
  e2. |
  
  f4 f f |
  \slurSolid
  e( d) d |
  \slurDashed
  d( e) c |
  c2.
  
  
  
  \slurDashed
  c4 d e |
  f2 c8( c) |
  d4( e) c |
  c2 e8( e) |
  
  f4( f) f |
  f4( f) f8( f) |
  f2 e4 |
  f2 f8( f) |
  
  f4( f) c4 |
  f4( f) f4 |
  \slurSolid d4( e) d4 |
  e2. |
  
  f4 f f |
  e( d) d |
  \slurDashed
  d( e) c |
  c2.
}
altoWords = \lyricmode {
  \dropLyricsIX
  \set stanza = #"1. "
  \set ignoreMelismata = ##t
  Je -- sus, our bro -- ther, kind _ and good, Was hum -- _ bly born in a sta -- ble rude,
  And the friend -- _ ly beasts _ a -- round _ Him stood; Je -- sus, our broth -- _ er, kind _ and good.
  
  \set stanza = #"4. "
  \set ignoreMelismata = ##t
  “I,” said the sheep with _ curl -- _ y horn, “I __ _ gave Him my wool _ for His blank -- et warm,
  He __ _ wore __ _ my coat _ on Christ -- _ mas morn.” “I,” said the sheep _ with curl -- _ y horn.
}
altoWordsII = \lyricmode {
  \dropLyricsIX
%\markup\italic
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  “I,” said the don -- key, shag -- gy and brown, “I car -- ried His Moth -- er up hill and down;
  I __ _ car -- ried Her safe -- ly to Beth -- le -- hem town.” “I,” said the don -- _ key, shag -- gy and brown.
  
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
  “I,” said the dove from the raf -- _ ters high,
  "" "" “Cooed Him to sleep, _ that He should not cry,
  We _ cooed Him to sleep, _ my mate _ and I.”
  “I,” said the dove _ from the raf -- ters high.
}
altoWordsIII = \lyricmode {
  \dropLyricsIX
  \set stanza = #"3. "
  \set ignoreMelismata = ##t
  “I,” said the cow, all white _ and red, “I gave Him my man -- ger __ _ for His bed,
  I __ _ gave Him my hay __ _ to pil -- low His head.” “I,” said the cow, _ all white _ and red.
  
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
  “I,” said the cam -- el, __ _ yel -- low and black,
  "" "" “O -- ver the des -- ert, up -- _ on my back
  I __ _ brought Him a gift in the Wise _ Men’s pack,”
  “I,” said the cam -- _ el, yel -- low and black.
}
altoWordsIV = \lyricmode {
  \dropLyricsIX
  \repeat unfold 32 \skip1
  \set stanza = #"7. "
  \set ignoreMelismata = ##t
  Thus ev -- ’ry beast by __ _ some _ good spell,
  In the sta -- _ ble dark _ was __ _ glad to tell
  Of the gift __ _ he gave _ Em -- man -- _ u -- el,
  The gift he gave _ Em -- man -- _ u -- el.
}
altoWordsV = \lyricmode {
}
altoWordsVI = \lyricmode {
}
altoWordsVII = \lyricmode {
}
tenorMusic = \relative c' {
  \slurDashed
  a4 a bes |
  c2 a4 |
  bes( bes) g |
  a2 g4 |
  
  a( a) a |
  \slurSolid
  bes( a) \slurDashed f8( f) |
  g2 g4 |
  a2 c8( c) |
  
  c4( c) c4 |
  bes4( bes) c4 |
  d4( d) d4 |
  cis2. |
  
  a4 a d |
  \slurSolid
  c( bes) a |
  \slurDashed
  bes( a) g |
  a2.
  
  
  
  \slurDashed
  a4 a bes |
  c2 a8( a) |
  bes4( bes) g |
  a2 g8( g) |
  
  a4( a) a |
  bes( a) f8( f) |
  g2 g4 |
  a2 c8( c) |
  
  c4( c) c4 |
  bes4( bes) c4 |
  d2 d4 |
  cis2. |
  
  a4 a d |
  \slurSolid
  c( bes) a |
  \slurDashed
  bes( a) g |
  a2.
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  \slurDashed
  f4 d c |
  f2 f,4 |
  bes4( c) c4 |
  f2 c4 |
  
  f( f) f |
  bes,2 d8( d) |
  c2 c4 |
  f2 f8( f) |
  
  f4( f) e4 |
  d4( d) f4 |
  bes,4( c) d4 |
  a2. |
  
  f4 f' f |
  c2 d4 |
  bes( c) c4 |
  f2.
  
  
  
  
  \slurDashed
  f4 d c |
  f2 f,8( f) |
  bes4( c) c4 |
  f2 c8( c) |
  
  f4( f) f |
  bes,4( bes) d8( d) |
  c2 c4 |
  f2 f8( f) |
  
  f4( f) e4 |
  d4( d) f4 |
  \slurSolid
  bes,4( c) d4 |
  a2. |
  
  f4 f' f |
  c2 d4 |
  \slurDashed
  bes( c) c4 |
  f2.
}
bassWords = \lyricmode {

}

\score {
  <<
   \new ChoirStaff <<
%    \new Lyrics = sopranos \with { \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) }
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . -0.5)) } \lyricsto "altos" \altoWordsVII
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . -0.5)) } \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . -0.5)) } \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . -0.5)) } \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . -0.5)) } \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . -0.5)) } \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . -0.5)) } \lyricsto "altos" \altoWords
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
%{IF_LESSER
\context {
  \Lyrics
  \override LyricText.font-size = #1.2
}
%}%END_IF_LESSER
%6.14 \context {\Lyrics\override LyricText.font-size = #0.8 }
    #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20)))
    \context {
      \Score
      \override SpacingSpanner.base-shortest-duration = #(ly:make-moment 1/2)
      \override SpacingSpanner.common-shortest-duration = #(ly:make-moment 1/2)
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
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"The Friendly Beasts"}}
    poet = \markup\oldStyleNum"Robert Davis (1881–1950)"
    composer = \markup\concat{"Adapted from "\italic"Orientis Partibus" \oldStyleNum", 12th Century French"}
    tagline = \markup { "from" \italic {HymnsAndCarolsOfChristmas.com}}
  }
}
%IF_NOT_LESSER
\markup \fill-line \center-align { \concat{ "from " \italic "HymnsAndCarolsOfChristmas.com"}}
\pageBreak












global = {
  \key f \major
  \time 4/4
  \autoBeamOff
}

sopMusic = \relative c' {
  \repeat volta 3 {
    f4. g8 a4 f |
    g e f2 |
    c'4 c d a8[ bes] |
    c4 c a2 |
    
    a4 g bes a |
    g f8[ g] a2 |
    c4. bes8 a4 f |
    g e f2
  }
  
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  c4. c8 f4 c |
  d c c2 |
  f4 f f f |
  f e f2 |
  
  f4 c f f |
  d d e2 |
  f4. f8 e4 d |
  d c c2
}
altoWords = \lyricmode {
  \dropLyricsIX
  \set stanza = #"1. "
  O -- ri -- én -- tis pár -- ti -- bus
  Ad -- ven -- tá -- vit á -- si -- nus,
  Pul -- cher et for -- tís -- si -- mus,
  Sár -- ci -- nis ap -- tís -- si -- mus.

  \set stanza = #"4. "
  Dum tra -- hit ve -- hí -- cu -- la,
  Mul -- ta cum sar -- cí -- nu -- la
  Il -- lí -- us man -- dí -- bu -- la
  Du -- ra te -- rit pá -- bu -- la.
}
altoWordsII = \lyricmode {
  \dropLyricsIX
%\markup\italic
  \set stanza = #"2. "
  Sal -- tu vin -- cit hín -- nu -- los
  Da -- mas et ca -- pré -- o -- los
  Su -- per dro -- me -- dá -- ri -- os
  Ve -- lox Ma -- di -- á -- ne -- os.
  
  \set stanza = #"5. "
  Cum a -- rís -- tis, hór -- de -- um
  Cóm -- e -- dit et cár -- du -- um
  Trí -- ti -- cum ex pá -- le -- a
  Sé -- gre -- gat in á -- re -- a.
}
altoWordsIII = \lyricmode {
  \dropLyricsIX
  \set stanza = #"3. "
  Hic in cól -- li -- bus Sy -- chen,
  Jam nu -- trí -- tus sub Ru -- ben
  Tráns -- i -- it per Jór -- da -- nem
  Sá -- li -- it in Béth -- le -- hem.
  
  \set stanza = #"6. "
  A -- men di -- cas, á -- si -- ne;
  Jam sa -- tur ex grá -- mi -- ne.
  A -- men, a -- men í -- te -- ra
  A -- sper -- ná -- re vé -- te -- ra.
}
altoWordsIV = \lyricmode {
}
altoWordsV = \lyricmode {
}
altoWordsVI = \lyricmode {
}
altoWordsVII = \lyricmode {
  \set stanza = #"7. "
}
tenorMusic = \relative c' {
  a4. bes8 c4 a |
  bes g a2 |
  a4 a bes f |
  g g a2 |
  
  c4 c bes c |
  d d cis2 |
  c?4. d8 c4 a |
  bes g a2
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  f4. e8 f4 f |
  bes, c f2 |
  f4 f bes, d |
  c c f2 |
  
  f4 e d c |
  bes bes a2 |
  a4. bes8 c4 d |
  bes c f2 
}
bassWords = \lyricmode {

}

\score {
  <<
   \new ChoirStaff <<
%    \new Lyrics = sopranos \with { \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) }
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \repeat unfold2\sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \repeat unfold2\altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . -0.5)) } \lyricsto "sopranos" \altoWordsVII
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . -0.5)) } \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . -0.5)) } \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . -0.5)) } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . -0.5)) } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . -0.5)) } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . -0.5)) } \lyricsto "sopranos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \repeat unfold2\tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \repeat unfold2\bassMusic >> }
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
      \override SpacingSpanner.common-shortest-duration = #(ly:make-moment 1/4)
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
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Orientis Partibus"}}
    poet = \markup\oldStyleNum"Attributed to Pierre de Corbeil, Bishop of Sens (d. 1222)"
    composer = \markup\oldStyleNum"12th Century French"
    tagline = \markup \concat{ "Words from " \italic"HymnsAndCarolsOfChristmas.com" ", Music from " \italic"CyberHymnal.org"}
  }
}
\markup \fill-line{\concat{ "Words from " \italic"HymnsAndCarolsOfChristmas.com" ", Music from " \italic"CyberHymnal.org"}}
\markup\vspace#0.5


















global = {
  \key g \major
  \time 3/4
  \autoBeamOff
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(-1 . 1)
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
  And one day be our guer -- don.
  
  \set stanza = #"6. "
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
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
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
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Although at Yule it Bloweth Cool"}}
    poet = \markup\oldStyleNum"George Ratcliffe Woodward (1848–1934)"
    composer = \markup \concat{\italic "Der wind der wet, der han der kret" \oldStyleNum", 1554"}
    arranger = \markup\oldStyleNum"Arranged by Charles Wood (1866–1926)"
    tagline = \markup \concat{ "from " \italic "The Cambridge Carol Book" \oldStyleNum", 1924"}
  }
}
\header{tagline = \markup \concat{ "from " \italic "The Cambridge Carol Book" \oldStyleNum", 1924"}}
%END_IF_NOT_LESSER
%{IF_LESSER
\header{tagline = \markup{ \concat{ "from " \italic "HymnsAndCarolsOfChristmas.com"}} }
%}%END_IF_LESSER

