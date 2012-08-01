-record('wine_grape',{
  'name'}).

-record('wine',{
  'name',
  'color',
  'flavor',
  'grape',
  'sugar'}).

-record('red-wine',{
  'name',
  'color' = 'red',
  'flavor',
  'grape',
  'sugar'}).

-record('white-wine',{
  'name',
  'color' = 'white',
  'flavor',
  'grape',
  'sugar'}).

-record('Chianti',{
  'name',
  'color' = 'red',
  'flavor',
  'grape',
  'sugar' = 'dry'}).

