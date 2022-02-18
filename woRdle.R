## Text-based wordle
generate_word <- function(wordlen=5,common=F){
  ## If common, uses the common word lists from the lexicon package
  if(common){
    wordbank <- unique(c(lexicon::sw_fry_1000,lexicon::sw_loughran_mcdonald_short,
                         lexicon::sw_dolch,lexicon::sw_mallet,lexicon::pos_action_verb,
                         lexicon::pos_preposition,ten_k_common_wordlist))
    return(toupper(sample(wordbank[which(nchar(wordbank) == wordlen)],size=1)))
  } else{
    ## If not common, draws from the entire scrabble dictionary
    return(toupper(sample(words::words[which(words::words$word_length == wordlen),1],size=1)))
  }
}

## Function for checking against true word
## returns -1 for right letter wrong place, 0 for wrong letter wrong place, 1 for right letter right place
check_word <- function(guess_word,true_word){
  wordlen <- nchar(guess_word)
  guess_list <- unlist(strsplit(guess_word,split=""))
  true_list <- unlist(strsplit(true_word,split=""))
  guess_result <- rep(0,wordlen)
  
  guess_freq <- table(guess_list)
  true_freq <- table(true_list)
  
  guess_letters <- names(guess_freq)
  
  freq_count <- rep(NA,wordlen)
  
  for(i in 1:length(guess_letters)){
    freq_count[guess_list==guess_letters[i]] <- 1:guess_freq[i]
  }
  
  for(i in 1:wordlen){
    if(guess_list[i] == true_list[i]){
      guess_result[i] <- 1
    }
  }
  for(i in 1:wordlen){
    if(guess_list[i] %in% true_list & guess_list[i] != true_list[i]){
      if(guess_freq[guess_list[i]] <= true_freq[guess_list[i]]){
        guess_result[i] <- -1
      } else{
        n_match <- sum(guess_result[true_list==guess_list[i]])
        if((freq_count[i] + n_match) <= true_freq[guess_list[i]]){
          guess_result[i] <- -1
        }
      }
    }
  }
  
  return(guess_result)
}

check_word_text <- function(guessed){
  ## Converts numeric word checking to a character for printing
  
  text.check <- rep(0,length(guessed))
  for(i in 1:length(guessed)){
    if(guessed[i] == -1){
      text.check[i] <- "/"
    } else if(guessed[i] == 1){
      text.check[i] <- "X"
    }
  }
  
  string.check <- paste0(text.check,collapse="")
  
  return(string.check)
}

remaining_letters <- function(guessed.letters,guessed.values){
  guessed.upper <- toupper(guessed.letters)
  ind.letters <- unlist(strsplit(guessed.upper,""))
  not.letters <- ind.letters[guessed.values==0]
  
  remaining <- LETTERS[!(LETTERS %in% not.letters)]
  
  return(remaining)
}

woRdle <- function(word.length=5,max.guesses=5,challenge=F){
  require(words)
  require(dplyr)
  require(lexicon)
  
  if((word.length > 7) | (word.length < 3)){
    return("Choose a word length between 3 and 7")
  }
  
  # Pick true words and start keeping track of previously guessed letters/words
  true.word <- toupper(generate_word(word.length,!challenge))
  n.guesses <- 0
  guessed.words <- ""
  guessed.checks <- c()
  
  if(challenge){
    wordbank <- toupper(words::words[which(words::words$word_length == wordlen),1])
  } else{
    wordbank <- toupper(unique(c(lexicon::sw_fry_1000,lexicon::sw_loughran_mcdonald_short,
                         lexicon::sw_dolch,lexicon::sw_mallet,lexicon::pos_action_verb,
                         lexicon::pos_preposition,ten_k_common_wordlist)))
  }
  
  print(paste0("Word Length: ", word.length,"; Max Guesses: ",max.guesses))
  print("0 = wrong letter; / = right letter, wrong place; X = right letter, right place")
  print("Enter q to quit")
  print("Use option challenge=T to use all scrabble words as a wordbank")
  
  while(n.guesses <= max.guesses){
    print(paste0("Guesses remaining: ",max.guesses - n.guesses))
    guess.val <- toupper(readline(prompt="GUESS: "))
    
    if(guess.val == "Q"){
      break
    }
    if(guess.val == true.word){
      print("YOU WIN!")
      break
    }
    if(nchar(guess.val) != nchar(true.word)){
      print("Incorrect number of characters")
      next
    }
    if(!(guess.val %in% wordbank)){
      print("Not in word bank")
      next
    }

    guess.check <- check_word_text(check_word(guess.val,true.word))
    print(guess.check)
    
    guessed.words <- paste0(guessed.words,guess.val,sep="")
    guessed.checks <- c(guessed.checks,check_word(guess.val,true.word))
    
    remaining.letters <- remaining_letters(guessed.words,guessed.checks)
    
    print(paste0("Remaining letters: ",paste0(remaining.letters,collapse=" ")))
    
    n.guesses <- n.guesses + 1
  }
  
  print(paste0("Answer: ",true.word,sep=""))
}

ten_k_common_wordlist <- c('site', 'news', 'online', 'click', 'price', 'email', 'health', 'data', 'policy',
                           'message', 'video', 'info', 'rights', 'public', 'books', 'years', 'privacy', 'items',
                           'user', 'mail', 'reviews', 'games', 'days', 'united', 'hotel', 'item', 'ebay',
                           'member', 'details', 'terms', 'hotels', 'local', 'results', 'posted', 'states', 'forum',
                           'based', 'prices', 'website', 'link', 'version', 'sports', 'county', 'photo', 'members',
                           'network', 'systems', 'access', 'posts', 'media', 'text', 'rating', 'account', 'times',
                           'sites', 'digital', 'profile', 'events', 'hours', 'image', 'title', 'quality', 'listing',
                           'content', 'private', 'tools', 'movies', 'college', 'article', 'york', 'jobs', 'source',
                           'sale', 'canada', 'teen', 'stock', 'credit', 'sales', 'english', 'estate', 'windows',
                           'photos', 'gallery', 'october', 'library', 'action', 'series', 'movie', 'forums', 'july',
                           'yahoo', 'medical', 'cart', 'staff', 'issues', 'users', 'topic', 'comment', 'things',
                           'working', 'mobile', 'blog', 'payment', 'login', 'offers', 'legal', 'stores', 'memory',
                           'social', 'options', 'rates', 'club', 'girls', 'latest', 'gift', 'texas', 'poker',
                           'status', 'browse', 'issue', 'seller', 'court', 'audio', 'groups', 'easy', 'files',
                           'making', 'areas', 'future', 'cards', 'london', 'meeting', 'schools', 'listed', 'popular',
                           'stories', 'journal', 'reports', 'central', 'images', 'council', 'archive', 'format', 'society',
                           'months', 'safety', 'friends', 'edition', 'cars', 'updated', 'studies', 'living', 'called',
                           'arts', 'limited', 'powered', 'means', 'daily', 'beach', 'average', 'records', 'costs',
                           'style', 'parts', 'works', 'federal', 'hosting', 'rules', 'adult', 'tickets', 'centre',
                           'cheap', 'kids', 'minutes', 'gifts', 'europe', 'reading', 'topics', 'tips', 'auto',
                           'videos', 'percent', 'global', 'tech', 'player', 'lyrics', 'germany', 'amount', 'deals',
                           'linux', 'points', 'error', 'camera', 'toys', 'golf', 'domain', 'methods', 'manager',
                           'models', 'cases', 'friday', 'annual', 'sony', 'shows', 'google', 'church', 'active',
                           'holiday', 'chat', 'writing', 'html', 'loss', 'brand', 'higher', 'effects', 'created',
                           'kingdom', 'french', 'storage', 'japan', 'loans', 'shoes', 'entry', 'orders', 'africa',
                           'summary', 'growth', 'notes', 'agency', 'monday', 'pics', 'western', 'income', 'cash',
                           'players', 'album', 'started', 'views', 'plans', 'types', 'lines', 'needed', 'printer',
                           'casino', 'menu', 'volume', 'mature', 'role', 'weeks', 'running', 'lower', 'union',
                           'jewelry', 'names', 'skills', 'bush', 'islands', 'advice', 'career', 'rental', 'british',
                           'teens', 'sellers', 'cable', 'values', 'coming', 'tuesday', 'lesbian', 'logo', 'nice',
                           'client', 'returns', 'shown', 'england', 'culture', 'choice', 'courses', 'airport', 'foreign',
                           'artist', 'levels', 'channel', 'mode', 'phones', 'ideas', 'button', 'homes', 'super',
                           'male', 'custom', 'located', 'asian', 'editor', 'cnet', 'rooms', 'female', 'primary',
                           'cancer', 'browser', 'purpose', 'feature', 'police', 'cameras', 'maps', 'ratings', 'chicago',
                           'forms', 'smith', 'wanted', 'unique', 'prior', 'sport', 'sources', 'mexico', 'regular',
                           'simply', 'paypal', 'option', 'rentals', 'hall', 'larger', 'anti', 'parents', 'nokia',
                           'impact', 'kitchen', 'wedding', 'owners', 'disease', 'paid', 'italy', 'classic', 'basis',
                           'cities', 'extra', 'rated', 'guides', 'maximum', 'amazon', 'warning', 'wine', 'vote',
                           'flowers', 'stars', 'lists', 'owner', 'retail', 'animals', 'ways', 'housing', 'takes',
                           'traffic', 'joined', 'agent', 'valid', 'senior', 'ireland', 'testing', 'trial', 'units',
                           'normal', 'ships', 'entire', 'leading', 'fitness', 'chinese', 'opinion', 'output', 'funds',
                           'greater', 'artists', 'java', 'guest', 'session', 'multi', 'fees', 'indian', 'prev',
                           'dating', 'pacific', 'mailing', 'vehicle', 'longer', 'panel', 'buying', 'default', 'iraq',
                           'boys', 'outdoor', 'protein', 'pool', 'mini', 'partner', 'authors', 'boards', 'faculty',
                           'parties', 'mission', 'goods', 'ability', 'moving', 'brands', 'places', 'spain', 'battery',
                           'youth', 'boston', 'debt', 'medium', 'core', 'sets', 'defined', 'papers', 'playing',
                           'awards', 'studio', 'reader', 'virtual', 'device', 'answers', 'rent', 'remote', 'offered',
                           'theory', 'minimum', 'visual', 'variety', 'isbn', 'agents', 'civil', 'songs', 'fixed',
                           'hands', 'finally', 'updates', 'desktop', 'classes', 'ohio', 'sector', 'jersey', 'fully',
                           'quotes', 'officer', 'driver', 'respect', 'unknown', 'worth', 'teacher', 'eyes', 'workers',
                           'peace', 'campus', 'showing', 'benefit', 'funding', 'devices', 'lord', 'fiction', 'watches',
                           'careers', 'museum', 'blogs', 'hits', 'zone', 'complex', 'spanish', 'setting', 'economy',
                           'highest', 'helpful', 'monthly', 'musical', 'laws', 'changed', 'russian', 'largest', 'african',
                           'titles', 'justice', 'bible', 'basket', 'applied', 'weekly', 'demand', 'suite', 'vegas',
                           'diet', 'army', 'auction', 'gear', 'allowed', 'selling', 'lots', 'firm', 'older',
                           'species', 'cells', 'module', 'resort', 'random', 'pricing', 'dvds', 'trading', 'calls',
                           'couple', 'vision', 'ending', 'clients', 'actions', 'naked', 'goal', 'markets', 'lowest',
                           'highly', 'lives', 'leather', 'palm', 'patient', 'actual', 'persons', 'tests', 'amateur',
                           'pain', 'xbox', 'factors', 'coffee', 'buyer', 'easily', 'oral', 'ford', 'poster',
                           'closed', 'pink', 'zealand', 'replies', 'shot', 'initial', 'canon', 'league', 'fishing',
                           'effort', 'phase', 'fields', 'fantasy', 'letters', 'motor', 'context', 'shirt', 'apparel',
                           'crime', 'breast', 'quickly', 'dollars', 'driving', 'surgery', 'patch', 'kansas', 'task',
                           'leader', 'servers', 'seconds', 'jones', 'arizona', 'keyword', 'flight', 'fuel', 'italian',
                           'saint', 'freedom', 'drugs', 'joint', 'premium', 'factor', 'growing', 'hearing', 'eastern',
                           'therapy', 'entries', 'dates', 'signed', 'upper', 'prime', 'samsung', 'limit', 'steps',
                           'errors', 'shops', 'efforts', 'creek', 'worked', 'urban', 'sorted', 'tours', 'labor',
                           'admin', 'nursing', 'defense', 'tags', 'covered', 'guys', 'expert', 'solid', 'orange',
                           'theme', 'guitar', 'finding', 'ipod', 'spirit', 'claims', 'affairs', 'goals', 'charges',
                           'reasons', 'magic', 'smart', 'talking', 'latin', 'oregon', 'birth', 'virus', 'quarter',
                           'tables', 'racing', 'facts', 'kong', 'plants', 'chain', 'avenue', 'missing', 'died',
                           'sitemap', 'moved', 'mental', 'viewed', 'centers', 'opening', 'recipes', 'gamma', 'plastic',
                           'truth', 'counter', 'failure', 'weekend', 'ontario', 'films', 'bridge', 'native', 'owned',
                           'played', 'readers', 'clubs', 'shirts', 'profit', 'leaders', 'posters', 'parking', 'russia',
                           'codes', 'kinds', 'seattle', 'teams', 'fort', 'senate', 'forces', 'turned', 'disc',
                           'named', 'theatre', 'laser', 'earlier', 'sponsor', 'icon', 'indiana', 'objects', 'ends',
                           'delete', 'nuclear', 'taxes', 'mouse', 'issued', 'sexual', 'false', 'passed', 'falls',
                           'soul', 'aids', 'stated', 'stats', 'hawaii', 'appears', 'decided', 'covers', 'designs',
                           'tourism', 'adults', 'clips', 'savings', 'graphic', 'binding', 'ended', 'winning', 'served',
                           'void', 'dining', 'atlanta', 'disk', 'credits', 'sweet', 'desk', 'pubmed', 'revenue',
                           'votes', 'duty', 'looked', 'flights', 'experts', 'signs', 'lack', 'depth', 'iowa',
                           'logged', 'laptop', 'vintage', 'concept', 'reality', 'forgot', 'origin', 'gaming', 'feeds',
                           'billion', 'faster', 'nations', 'broken', 'alaska', 'anime', 'query', 'equity', 'rural',
                           'shared', 'sounds', 'tape', 'spam', 'acid', 'bytes', 'forced', 'height', 'null',
                           'speaker', 'filed', 'offices', 'managed', 'failed', 'korea', 'banks', 'secret', 'bath',
                           'leads', 'toronto', 'theater', 'springs', 'healthy', 'font', 'assets', 'injury', 'drivers',
                           'lawyer', 'figures', 'married', 'sharing', 'portal', 'waiting', 'beta', 'gratis', 'banking',
                           'calling', 'jazz', 'serving', 'bags', 'miami', 'comics', 'matters', 'houses', 'postal',
                           'wales', 'minor', 'noted', 'reduced', 'physics', 'rare', 'spent', 'extreme', 'samples',
                           'bars', 'removed', 'helps', 'singles', 'amounts', 'dual', 'brazil', 'static', 'scene',
                           'writer', 'fans', 'academy', 'dynamic', 'gender', 'colour', 'vendor', 'intel', 'bids',
                           'regions', 'toll', 'cape', 'rings', 'meaning', 'ladies', 'ticket', 'agreed', 'soccer',
                           'math', 'posting', 'viewing', 'christ', 'dogs', 'aspects', 'austria', 'ahead', 'scheme',
                           'utility', 'preview', 'manner', 'matrix', 'devel', 'turkey', 'degrees', 'seeking', 'inches',
                           'phoenix', 'shares', 'colors', 'wars', 'cisco', 'appeal', 'cruise', 'bonus', 'disney',
                           'adobe', 'smoking', 'drives', 'arms', 'alabama', 'trees', 'dealer', 'utah', 'nearby',
                           'carried', 'miller', 'clothes', 'caused', 'luxury', 'babes', 'frames', 'circuit', 'layer',
                           'printed', 'removal', 'easier', 'faqs', 'adding', 'prints', 'factory', 'revised', 'optical',
                           'amazing', 'suites', 'feeling', 'hidden', 'serial', 'relief', 'ratio', 'copies', 'recipe',
                           'proof', 'diff', 'tennis', 'bass', 'bedroom', 'pets', 'bureau', 'maine', 'ideal',
                           'specs', 'pieces', 'parks', 'dinner', 'lawyers', 'stress', 'cream', 'runs', 'trends',
                           'yeah', 'boxes', 'hills', 'fourth', 'advisor', 'evil', 'aware', 'remains', 'firms',
                           'euro', 'generic', 'usage', 'charts', 'mixed', 'census', 'peak', 'transit', 'compact',
                           'poetry', 'lights', 'keeping', 'matches', 'width', 'engines', 'array', 'climate', 'alcohol',
                           'greek', 'walking', 'smaller', 'newest', 'extent', 'export', 'modules', 'sweden', 'backup',
                           'holding', 'ages', 'affect', 'virgin', 'raised', 'dealers', 'helping', 'perl', 'bike',
                           'totally', 'plate', 'blonde', 'organic', 'albums', 'cheats', 'guests', 'hosted', 'kits',
                           'agenda', 'tracks', 'logic', 'grants', 'leaving', 'cooking', 'sizes', 'entered', 'iran',
                           'keys', 'costa', 'belgium', 'holy', 'acts', 'mesh', 'trail', 'crafts', 'highway',
                           'setup', 'poll', 'booking', 'fiscal', 'styles', 'unix', 'filled', 'bond', 'blues',
                           'portion', 'scope', 'cables', 'biology', 'dental', 'killed', 'border', 'ancient', 'starts',
                           'leisure', 'learned', 'opened', 'husband', 'crazy', 'britain', 'concert', 'scores', 'comedy',
                           'weblog', 'linear', 'bears', 'carrier', 'edited', 'visa', 'jewish', 'meter', 'linked',
                           'pure', 'lessons', 'begins', 'reform', 'lens', 'alerts', 'treated', 'mysql', 'offline',
                           'leaves', 'babe', 'checks', 'reached', 'safari', 'crew', 'legs', 'enabled', 'genre',
                           'montana', 'tested', 'rear', 'bound', 'adapter', 'node', 'formal', 'hockey', 'storm',
                           'micro', 'laptops', 'showed', 'editors', 'mens', 'threads', 'bowl', 'supreme', 'tank',
                           'dolls', 'navy', 'cancel', 'limits', 'weapons', 'outlet', 'czech', 'ultra', 'winner',
                           'idaho', 'episode', 'potter', 'dish', 'plays', 'oxford', 'patent', 'slot', 'eating',
                           'planned', 'lodge', 'mirror', 'kernel', 'stocks', 'buyers', 'charged', 'taiwan', 'chosen',
                           'demo', 'greece', 'swiss', 'labour', 'nights', 'behalf', 'rice', 'loop', 'salary',
                           'foods', 'gourmet', 'orleans', 'empire', 'resume', 'newly', 'avatar', 'illegal', 'rome',
                           'arab', 'helped', 'premier', 'consent', 'drama', 'contest', 'bands', 'boot', 'lunch',
                           'chamber', 'guinea', 'muscle', 'polls', 'typical', 'tower', 'misc', 'chicken', 'shower',
                           'sending', 'tonight', 'holdem', 'beer', 'spyware', 'formula', 'solar', 'finder', 'unable',
                           'periods', 'tasks', 'attacks', 'const', 'doors', 'resorts', 'biggest', 'visitor', 'twin',
                           'gateway', 'alumni', 'drawing', 'ordered', 'romance', 'themes', 'powers', 'heaven', 'bits',
                           'focused', 'egypt', 'norway', 'vermont', 'blocks', 'hunting', 'chip', 'bodies', 'cutting',
                           'writers', 'marks', 'loved', 'mapping', 'birds', 'char', 'indexed', 'saved', 'paying',
                           'cartoon', 'shots', 'moore', 'granted', 'choices', 'carbon', 'crisis', 'outlook', 'massive',
                           'denmark', 'header', 'poverty', 'formed', 'piano', 'grid', 'sheets', 'puerto', 'plasma',
                           'voip', 'mystery', 'journey', 'bidding', 'risks', 'banner', 'charter', 'ports', 'dreams',
                           'blogger', 'stands', 'rapid', 'hairy', 'reverse', 'deposit', 'seminar', 'nasa', 'wheels',
                           'dutch', 'formats', 'depends', 'boots', 'holds', 'router', 'editing', 'poland', 'folder',
                           'womens', 'upload', 'pulse', 'voting', 'courts', 'notices', 'detroit', 'metro', 'toshiba',
                           'plot', 'airline', 'regard', 'exists', 'smooth', 'narrow', 'threat', 'surveys', 'sitting',
                           'putting', 'vietnam', 'trailer', 'castle', 'gardens', 'missed', 'antique', 'labels', 'acting',
                           'heads', 'stored', 'exam', 'logos', 'density', 'beds', 'mention', 'grey', 'honda',
                           'amended', 'bills', 'bold', 'stable', 'opera', 'doctors', 'lesson', 'cinema', 'asset',
                           'blank', 'severe', 'deluxe', 'humor', 'aged', 'lived', 'bulk', 'fabric', 'visits',
                           'tight', 'domains', 'pmid', 'flying', 'berlin', 'cute', 'para', 'siemens', 'pounds',
                           'buffalo', 'camping', 'meets', 'welfare', 'peer', 'marked', 'driven', 'medline', 'bottle',
                           'rubber', 'closing', 'tampa', 'legend', 'adams', 'python', 'monster', 'villa', 'columns',
                           'bugs', 'cookies', 'entity', 'cruises', 'gate', 'holder', 'duties', 'ethics', 'forever',
                           'dragon', 'brings', 'heating', 'scripts', 'stereo', 'dealing', 'commit', 'liberal', 'livecam',
                           'trips', 'sides', 'turns', 'cache', 'belt', 'jacket', 'oracle', 'lease', 'hobbies',
                           'excess', 'console', 'giant', 'shipped', 'seats', 'alarm', 'voltage', 'loading', 'stamps',
                           'vinyl', 'mining', 'ongoing', 'worst', 'imaging', 'betting', 'wyoming', 'analyst', 'garage',
                           'thongs', 'finland', 'derived', 'honor', 'eagle', 'pants', 'prayer', 'luck', 'postage',
                           'dial', 'cheese', 'comic', 'crown', 'maker', 'picks', 'gang', 'fetish', 'applies',
                           'casinos', 'apache', 'filters', 'craft', 'cake', 'fellow', 'lounge', 'semi', 'coins',
                           'gross', 'cafe', 'horror', 'capable', 'debian', 'epson', 'elected', 'victory', 'ethnic',
                           'actor', 'finds', 'citizen', 'prize', 'occurs', 'anytime', 'lies', 'pipe', 'layout',
                           'horses', 'dirty', 'donate', 'worker', 'alive', 'wings', 'breaks', 'genetic', 'waters',
                           'ridge', 'cabinet', 'modem', 'sick', 'dose', 'toyota', 'streets', 'vector', 'shaved',
                           'turning', 'buffer', 'purple', 'mutual', 'syntax', 'prison', 'chairs', 'moves', 'inquiry',
                           'checked', 'trend', 'visible', 'cats', 'oldest', 'rhode', 'mercury', 'worse', 'summit',
                           'victims', 'spaces', 'burning', 'coupons', 'cialis', 'boats', 'glance', 'arcade', 'tells',
                           'obvious', 'fiber', 'graph', 'talks', 'filing', 'passing', 'awarded', 'trials', 'tissue',
                           'masters', 'bonds', 'folk', 'commons', 'fraud', 'arrival', 'pottery', 'aspect', 'awesome',
                           'mexican', 'counts', 'priced', 'hist', 'desired', 'inter', 'closer', 'assumes', 'heights',
                           'shadow', 'riding', 'firefox', 'expense', 'grove', 'venture', 'clinic', 'korean', 'healing',
                           'mall', 'packet', 'studios', 'buttons', 'funded', 'winners', 'roads', 'dublin', 'rolling',
                           'arrived', 'creates', 'faces', 'tourist', 'mayor', 'senator', 'grades', 'digest', 'lodging',
                           'tion', 'wiki', 'radar', 'losses', 'combat', 'stopped', 'lakes', 'closely', 'diary',
                           'kings', 'adds', 'flags', 'baker', 'walls', 'abroad', 'drawn', 'visited', 'roof',
                           'beast', 'targets', 'pizza', 'invited', 'yards', 'farmers', 'queries', 'ukraine', 'absence',
                           'nearest', 'cluster', 'vendors', 'mpeg', 'yoga', 'serves', 'woods', 'lamp', 'partial',
                           'couples', 'ranking', 'jokes', 'http', 'simpson', 'twiki', 'sublime', 'palace', 'wins',
                           'globe', 'trusted', 'copper', 'rack', 'dicke', 'receipt', 'ghost', 'boss', 'pride',
                           'knowing', 'cloudy', 'chile', 'plenty', 'solo', 'throat', 'uniform', 'wealth', 'vacuum',
                           'dancing', 'brass', 'writes', 'plaza', 'pdas', 'quest', 'trans', 'booty', 'acrobat',
                           'plates', 'acres', 'venue', 'thermal', 'essays', 'vital', 'telling', 'fairly', 'coastal',
                           'config', 'excel', 'modes', 'stupid', 'harbor', 'hungary', 'puzzle', 'rising', 'wells',
                           'opens', 'insight', 'secrets', 'lucky', 'philips', 'penalty', 'glasses', 'enables', 'iraqi',
                           'builder', 'vista', 'chips', 'foto', 'arena', 'pupils', 'tabs', 'outcome', 'casual',
                           'grown', 'lovely', 'extras', 'centres', 'clause', 'lands', 'troops', 'indoor', 'armed',
                           'broker', 'charger', 'cooling', 'gulf', 'trucks', 'divorce', 'shopper', 'tokyo', 'partly',
                           'nikon', 'pills', 'tiger', 'folks', 'sensor', 'exposed', 'telecom', 'angels', 'deputy',
                           'sealed', 'thai', 'loaded', 'scenes', 'mill', 'founded', 'chronic', 'icons', 'moral',
                           'trained', 'roses', 'labs', 'tobacco', 'wooden', 'motors', 'tough', 'roberts', 'gonna',
                           'chest', 'pension', 'worship', 'damages', 'shorts', 'diverse', 'sole', 'facing', 'tones',
                           'passion', 'laid', 'defence', 'patches', 'weak', 'refund', 'towns', 'trembl', 'divided',
                           'blvd', 'wise', 'emails', 'cyprus', 'odds', 'insider', 'makers', 'hearts', 'legacy',
                           'pleased', 'vitamin', 'widely', 'genuine', 'raising', 'hybrid', 'reads', 'roles', 'sons',
                           'leaf', 'bigger', 'billing', 'diesel', 'saudi', 'fault', 'cuba', 'silk', 'babies',
                           'bristol', 'compaq', 'wolf', 'slowly', 'rugby', 'infant', 'sectors', 'fluid', 'grounds',
                           'fits', 'meal', 'baskets', 'wright', 'proven', 'cached', 'studied', 'profits', 'devil',
                           'comply', 'florist', 'deutsch', 'webcam', 'cuts', 'funeral', 'nutten', 'enjoyed', 'quebec',
                           'mars', 'sized', 'manga', 'noticed', 'socket', 'signals', 'caps', 'pill', 'theft',
                           'symbols', 'humans', 'analog', 'facial', 'talent', 'dated', 'seeker', 'wisdom', 'mint',
                           'packard', 'payday', 'elite', 'holders', 'swedish', 'poems', 'robot', 'collins', 'stages',
                           'winds', 'powder', 'stones', 'gnome', 'roots', 'losing', 'gadgets', 'glasgow', 'impacts',
                           'gospel', 'loves', 'induced', 'knight', 'loose', 'aims', 'linking', 'appeals', 'earned',
                           'illness', 'islamic', 'ieee', 'lebanon', 'corp', 'kennedy', 'teenage', 'soap', 'cooper',
                           'secured', 'unusual', 'slots', 'routine', 'toolbar', 'rocks', 'titans', 'wearing', 'axis',
                           'sought', 'genes', 'mounted', 'habitat', 'median', 'guns', 'scanner', 'hero', 'integer',
                           'engaged', 'falling', 'basics', 'carpet', 'struct', 'lenses', 'binary', 'punk', 'dropped',
                           'duke', 'wage', 'hosts', 'moments', 'atlas', 'strings', 'feels', 'torture', 'deleted',
                           'rica', 'inkjet', 'wizard', 'corps', 'actors', 'liver', 'liable', 'eminem', 'antenna',
                           'picked', 'assumed', 'belief', 'killing', 'bikini', 'memphis', 'decor', 'lookup', 'texts',
                           'harvard', 'brokers', 'ottawa', 'doll', 'podcast', 'seasons', 'peru', 'bidder', 'singer',
                           'evans', 'herald', 'fails', 'aging', 'nike', 'plugin', 'diving', 'latinas', 'terror',
                           'younger', 'mice', 'rapidly', 'temp', 'intro', 'clerk', 'vast', 'mills', 'holland',
                           'jeans', 'fonts', 'refers', 'mood', 'quiz', 'sigma', 'xhtml', 'victim', 'demands',
                           'careful', 'sunset', 'tracked', 'minimal', 'lottery', 'tops', 'framed', 'licence', 'essay',
                           'camps', 'packs', 'romania', 'ncaa', 'thou', 'greatly', 'mask', 'cycling', 'turkish',
                           'coal', 'pentium', 'quantum', 'intent', 'largely', 'arrow', 'rough', 'weird', 'lion',
                           'holes', 'blade', 'cookie', 'meals', 'canyon', 'goto', 'meters', 'passes', 'pointer',
                           'durham', 'permits', 'muslim', 'sleeve', 'cleaner', 'cricket', 'beef', 'feeding', 'hats',
                           'surf', 'olympic', 'customs', 'rainbow', 'decline', 'gloves', 'israeli', 'cord', 'skiing',
                           'valve', 'hewlett', 'proceed', 'flickr', 'jamaica', 'shelf', 'timing', 'denied', 'fotos',
                           'outer', 'deaths', 'rivers', 'tales', 'islam', 'nodes', 'thumbs', 'seeds', 'cited',
                           'lite', 'skype', 'twelve', 'founder', 'decade', 'dispute', 'tired', 'titten', 'adverse',
                           'excerpt', 'drinks', 'voices', 'acute', 'tons', 'perfume', 'honest', 'albany', 'creator',
                           'museums', 'coding', 'tracker', 'passage', 'trunk', 'hiking', 'jelsoft', 'headset', 'oakland',
                           'waves', 'camel', 'lamps', 'hood', 'suicide', 'arabia', 'juice', 'logical', 'sauce',
                           'fame', 'panama', 'payable', 'athens', 'judges', 'retired', 'remarks', 'decades', 'walked',
                           'arising', 'nissan', 'eggs', 'railway', 'pointed', 'causing', 'norton', 'locked', 'fusion',
                           'mineral', 'beads', 'fortune', 'canvas', 'parish', 'claimed', 'screens', 'planner', 'croatia',
                           'flows', 'stadium', 'mins', 'fewer', 'coupon', 'nurses', 'stem', 'proxy', 'lanka',
                           'edwards', 'costume', 'tagged', 'voted', 'killer', 'bikes', 'gates', 'tune', 'bishop',
                           'pulled', 'shaped', 'farmer', 'puts', 'norfolk', 'trek', 'heroes', 'painted', 'lycos',
                           'zdnet', 'artwork', 'ethical', 'floral', 'ties', 'schemes', 'neutral', 'fisher', 'spears',
                           'bedding', 'joining', 'heading', 'equally', 'bearing', 'combo', 'seniors', 'worlds', 'guilty',
                           'haven', 'tablet', 'jury', 'charm', 'lawn', 'violent', 'basin', 'soup', 'ranch',
                           'cottage', 'drunk', 'crimes', 'mozilla', 'byte', 'toner', 'latex', 'anymore', 'oclc',
                           'delhi', 'alien', 'locator', 'nepal', 'moscow', 'thesis', 'jews', 'nylon', 'discs',
                           'trim', 'nigeria', 'ceiling', 'pixels', 'espn', 'fleet', 'bunch', 'totals', 'singing',
                           'optimal', 'lung', 'turner', 'sucking', 'cents', 'reuters', 'spoken', 'stayed', 'civic',
                           'manuals', 'sees', 'watched', 'saver', 'thereof', 'grill', 'rogers', 'grain', 'regime',
                           'wanna', 'wishes', 'bull', 'ranging', 'repairs', 'breath', 'mart', 'candle', 'hanging',
                           'colored', 'tale', 'seeks', 'herbal', 'loving', 'routing', 'docs', 'elegant', 'gains',
                           'renewal', 'opposed', 'deemed', 'scoring', 'sisters', 'critics', 'spots', 'hacker', 'madrid',
                           'margin', 'coin', 'solely', 'salon', 'turbo', 'headed', 'voters', 'arch', 'murphy',
                           'thinks', 'hdtv', 'asin', 'aimed', 'mirrors', 'tricks', 'reset', 'panels', 'deer',
                           'kodak', 'tongue', 'bowling', 'danish', 'monkey', 'invest', 'lovers', 'atomic', 'approx',
                           'arabic', 'faced', 'solving', 'oven', 'chains', 'sixth', 'deviant', 'quoted', 'farms',
                           'imports', 'bronze', 'macro', 'sender', 'crucial', 'tuition', 'spouse', 'exotic', 'viewer',
                           'signup', 'threats', 'puzzles', 'damaged', 'cams', 'autos', 'proved', 'dozen', 'cloth',
                           'lotus', 'salmon', 'olympus', 'proc', 'cargo', 'salem', 'starter', 'likes', 'butter',
                           'pepper', 'weapon', 'luggage', 'burden', 'chef', 'tapes', 'zones', 'races', 'isle',
                           'stylish', 'slim', 'grocery', 'depot', 'comp', 'blend', 'spec', 'finest', 'realty',
                           'penn', 'phpbb', 'midi', 'toilet', 'ranked', 'jackets', 'routes', 'packed', 'excited',
                           'recover', 'tied', 'lopez', 'timely', 'talked', 'delayed', 'villas', 'ebook', 'exclude',
                           'peeing', 'oils', 'sept', 'anxiety', 'bingo', 'whilst', 'spatial', 'ceramic', 'minds',
                           'xanax', 'pays', 'fingers', 'ebooks', 'leeds', 'cedar', 'stud', 'hopes', 'burns',
                           'pumps', 'beijing', 'peoples', 'utils', 'advised', 'spider', 'phys', 'ranges', 'pairs',
                           'trails', 'hudson', 'calgary', 'interim', 'divine', 'chose', 'dialog', 'venues', 'calcium',
                           'newport', 'pole', 'indians', 'harvest', 'prague', 'locally', 'pickup', 'mothers', 'nascar',
                           'iceland', 'candles', 'mega', 'sailing', 'moms', 'sacred', 'morocco', 'chrome', 'refused',
                           'ecology', 'congo', 'delays', 'cyber', 'verizon', 'scored', 'clone', 'lambda', 'relay',
                           'tears', 'oasis', 'angry', 'silicon', 'lover', 'beatles', 'lips', 'pond', 'rolls',
                           'thomson', 'barnes', 'malta', 'daddy', 'ferry', 'rabbit', 'seating', 'exports', 'omaha',
                           'loads', 'motel', 'unions', 'warrant', 'solaris', 'frozen', 'royalty', 'scales', 'strain',
                           'ripe', 'yamaha', 'hebrew', 'gained', 'dying', 'laundry', 'stuck', 'placing', 'stops',
                           'filling', 'imposed', 'silence', 'scsi', 'focuses', 'soviet', 'treaty', 'vocal', 'trainer',
                           'volumes', 'lemon', 'toxic', 'nuts', 'bizrate', 'vienna', 'implied', 'span', 'packing',
                           'statute', 'chapel', 'layers', 'guided', 'bahamas', 'powell', 'mixture', 'bench', 'univ',
                           'rider', 'radius', 'logging', 'hampton', 'borders', 'pads', 'butts', 'inns', 'sheep',
                           'wines', 'nursery', 'harder', 'cups', 'salad', 'tender', 'foam', 'clouds', 'staying',
                           'estonia', 'veteran', 'streams', 'landing', 'signing', 'asks', 'namibia', 'prairie', 'reunion',
                           'bean', 'sword', 'absent', 'sells', 'ecuador', 'hoping', 'spirits', 'pixel', 'bias',
                           'paths', 'tribune', 'vessel', 'acids', 'viruses', 'cheaper', 'dairy', 'samoa', 'leasing',
                           'beliefs', 'squad', 'scroll', 'wages', 'forests', 'nano', 'invalid', 'martial', 'males',
                           'colours', 'tunnel', 'genres', 'patents', 'chaos', 'wheat', 'beaver', 'kijiji', 'eagles',
                           'bases', 'accused', 'unity', 'bride', 'rats', 'defines', 'begun', 'packets', 'anchor',
                           'socks', 'stat', 'trigger', 'essex', 'beaches', 'folders', 'routers', 'pendant', 'dresses',
                           'baptist', 'hiring', 'clocks', 'bios', 'females', 'fever', 'cuisine', 'surely', 'myspace',
                           'theorem', 'thee', 'stylus', 'pope', 'drums', 'jeep', 'chicks', 'cattle', 'radical',
                           'rover', 'reload', 'flame', 'levitra', 'tanks', 'elderly', 'mono', 'tile', 'bolivia',
                           'hottest', 'stevens', 'kuwait', 'alleged', 'webster', 'apps', 'bridal', 'annex', 'tribal',
                           'curious', 'freight', 'rebate', 'meetup', 'eclipse', 'sudan', 'shuttle', 'cycles', 'affects',
                           'ciao', 'ampland', 'knee', 'prep', 'chem', 'fastest', 'butler', 'injured', 'payroll',
                           'courier', 'hints', 'pros', 'techno', 'tribute', 'wired', 'immune', 'latvia', 'rarely',
                           'barrier', 'trains', 'metals', 'bicycle', 'letting', 'celtic', 'bottles', 'boxing', 'bangkok',
                           'hughes', 'chess', 'survive', 'menus', 'canal', 'amino', 'herbs', 'clinics', 'watson',
                           'lying', 'strict', 'saddam', 'offense', 'protest', 'hobby', 'fiji', 'inline', 'washing',
                           'audi', 'enquiry', 'closure', 'timber', 'volt', 'intense', 'showers', 'ruling', 'steady',
                           'dirt', 'myers', 'drops', 'wider', 'plugins', 'sensors', 'hourly', 'blame', 'freebsd',
                           'acer', 'dist', 'handed', 'intake', 'tucson', 'heavily', 'headers', 'geek', 'uncle',
                           'devoted', 'sodium', 'hormone', 'brick', 'naval', 'bridges', 'watt', 'thehun', 'decent',
                           'casting', 'dayton', 'shortly', 'pins', 'reno', 'warrior', 'diploma', 'cabin', 'polo',
                           'valium', 'copying', 'horn', 'uganda', 'fired', 'prot', 'trivia', 'adidas', 'perth',
                           'frog', 'grammar', 'syria', 'klein', 'tires', 'logs', 'hazard', 'retro', 'boolean',
                           'suits', 'chances', 'bizarre', 'fruits', 'ribbon', 'jpeg', 'startup', 'suzuki', 'kissing',
                           'handy', 'exempt', 'crops', 'reduces', 'guild', 'capitol', 'dishes', 'nervous', 'extends',
                           'replica', 'tribe', 'trades', 'superb', 'nuke', 'handled', 'legends', 'boom', 'calm',
                           'floors', 'exhaust', 'speaks', 'copied', 'scotia', 'farming', 'gibson', 'fork', 'roller',
                           'batch', 'latino', 'ghana', 'edges', 'mixing', 'handles', 'skilled', 'fitted', 'asthma',
                           'twins', 'zope', 'reward', 'windsor', 'zambia', 'gmbh', 'sims', 'tray', 'inputs',
                           'genome', 'escorts', 'thong', 'medal', 'coaches', 'vessels', 'harbour', 'walks', 'knives',
                           'honors', 'booth', 'indie', 'unified', 'bones', 'polar', 'fallen', 'precise', 'sussex',
                           'msgid', 'invoice', 'suse', 'backed', 'motels', 'forming', 'embassy', 'cave', 'slight',
                           'wool', 'msgstr', 'adipex', 'horizon', 'deeply', 'toolbox', 'prizes', 'bosnia', 'patio',
                           'surfing', 'optics', 'eyed', 'beans', 'disable', 'snake', 'lending', 'oops', 'plains',
                           'midwest', 'karaoke', 'lonely', 'racial', 'bermuda', 'mobiles', 'kelkoo', 'dies', 'terrace',
                           'seafood', 'novels', 'safely', 'finite', 'kidney', 'fixes', 'sends', 'durable', 'mazda',
                           'allied', 'throws', 'roster', 'wichita', 'nasdaq', 'uruguay', 'timer', 'tablets', 'tuning',
                           'futures', 'verse', 'highs', 'custody', 'ipaq', 'comm', 'rocket', 'bullet', 'towers',
                           'racks', 'lace', 'nasty', 'tumor', 'ugly', 'watts', 'hart', 'ment', 'tubes',
                           'priest', 'trance', 'locale', 'biol', 'bundle', 'runner', 'rows', 'notion', 'skins',
                           'mailed', 'fujitsu', 'arctic', 'exams', 'rewards', 'treo', 'seventh', 'gods', 'welsh',
                           'belly', 'stolen', 'soonest', 'haiti', 'poly', 'ears', 'fist', 'lenders', 'fitting',
                           'mere', 'agrees', 'cons', 'surplus', 'elder', 'sonic', 'cheers', 'taxi', 'belarus',
                           'zoning', 'gravity', 'thumb', 'guitars', 'essence', 'mighty', 'holmes', 'galaxy', 'caring',
                           'worn', 'shaw', 'expo', 'itunes', 'stomach', 'buried', 'newbie', 'ranks', 'debut',
                           'deny', 'anatomy', 'bali', 'trio', 'cube', 'defects', 'marker', 'clarity', 'rugs',
                           'monaco', 'settled', 'folding', 'airfare', 'vaccine', 'belize', 'fate', 'volvo', 'robust',
                           'minolta', 'gras', 'jungle', 'alpine', 'andale', 'remix', 'alias', 'newer', 'spice',
                           'oval', 'implies', 'soma', 'cooler', 'ascii', 'donor', 'tension', 'href', 'benz',
                           'trash', 'shapes', 'wifi', 'tier', 'manor', 'breeds', 'rapids', 'disco', 'endif',
                           'lexmark', 'eternal', 'guam', 'cite', 'metric', 'hotmail', 'armenia', 'varied', 'grande',
                           'closest', 'actress', 'mess', 'tigers', 'slides', 'lender', 'chorus', 'rhythm', 'digit',
                           'argued', 'dietary', 'clarke', 'lions', 'findlaw', 'pools', 'lyric', 'speeds', 'matched',
                           'dump', 'warming', 'vocals', 'chubby', 'grave', 'burner', 'finnish', 'deeper', 'muslims',
                           'hose', 'footage', 'howto', 'worthy', 'reveals', 'saints', 'carries', 'saves', 'tunisia',
                           'gotta', 'cowboy', 'bahrain', 'queens', 'pubs', 'tribes', 'defeat', 'clicks', 'naughty',
                           'hazards', 'insured', 'harper', 'mardi', 'tenant', 'tattoo', 'algebra', 'shadows', 'silly',
                           'freely', 'sunrise', 'mild', 'weblogs', 'belongs', 'readily', 'nudist', 'ensures', 'clan',
                           'legally', 'shame', 'sync', 'mesa', 'fatal', 'remedy', 'briefly', 'genius', 'fighter',
                           'flesh', 'adapted', 'barely', 'estates', 'borough', 'andrews', 'marble', 'hull', 'surrey',
                           'belkin', 'modular', 'giants', 'balloon', 'memo', 'solved', 'tide', 'gratuit', 'funk',
                           'qatar', 'cayman', 'jaguar', 'sheer', 'posing', 'rand', 'hopkins', 'urgent', 'infants',
                           'gothic', 'witch', 'cohen', 'usgs', 'puppy', 'acre', 'graphs', 'revenge', 'expires',
                           'enemies', 'lows', 'aqua', 'chen', 'accepts', 'pest', 'roughly', 'sticker', 'reef',
                           'satin', 'mailto', 'promo', 'worried', 'tunes', 'garbage', 'phrases', 'boring', 'reaches',
                           'schema', 'sofa', 'quizzes', 'prefix', 'barrel', 'typing', 'nerve', 'dans', 'planets',
                           'deficit', 'boulder', 'coupled', 'viii', 'myanmar', 'floppy', 'texture', 'antigua', 'thunder',
                           'tent', 'caution', 'locks', 'dept', 'euros', 'pirates', 'aerial', 'hawk', 'rebel',
                           'origins', 'hired', 'makeup', 'textile', 'lamb', 'tobago', 'indexes', 'hindu', 'licking',
                           'markers', 'weights', 'albania', 'lasting', 'wicked', 'kills', 'webcams', 'pushed', 'junk',
                           'slope', 'reggae', 'poet', 'surname', 'nails', 'evident', 'rides', 'rehab', 'epic',
                           'saturn', 'allergy', 'sake', 'twisted', 'merit', 'enzyme', 'zshops', 'planes', 'disks',
                           'condo', 'pokemon', 'ambien', 'builds', 'shaft', 'casio', 'dude', 'fires', 'algeria',
                           'blessed', 'cardiff', 'favors', 'potato', 'sticks', 'reforms', 'onion', 'strand', 'lawsuit',
                           'alto', 'cheque', 'banners', 'circles', 'italic', 'beats', 'scuba', 'gore', 'cult',
                           'passive', 'valued', 'cage', 'courage', 'verde', 'gazette', 'hitachi', 'divx', 'batman',
                           'eval', 'anaheim', 'dried', 'knights', 'flux', 'derby', 'altered', 'pontiac', 'scenic',
                           'muze', 'sewing', 'munich', 'oman', 'celebs', 'lighter', 'rage', 'adsl', 'prix',
                           'tactics', 'trusts', 'pillow', 'shorter', 'relying', 'finals', 'parcel', 'refined', 'fifteen',
                           'fears', 'acrylic', 'rolled', 'tuner', 'avon', 'rays', 'toddler', 'flavor', 'alike',
                           'walt', 'hungry', 'acne', 'blocked', 'libs', 'malawi', 'sagem', 'halo', 'strikes',
                           'lesser', 'gays', 'dressed', 'belfast', 'exec', 'dealt', 'niagara', 'charms', 'trader',
                           'bucks', 'denial', 'thrown', 'prepaid', 'raises', 'electro', 'badge', 'wrist', 'ballot',
                           'lexus', 'varying', 'trustee', 'maui', 'angola', 'realm', 'helmet', 'yemen', 'tsunami',
                           'scholar', 'nickel', 'buses', 'expedia', 'geology', 'coating', 'wallet', 'cleared', 'smilies',
                           'vids', 'boating', 'corners', 'broader', 'rouge', 'yeast', 'yale', 'coated', 'doom',
                           'hitting', 'yukon', 'beings', 'issn', 'aquatic', 'habits', 'myth', 'singh', 'ferrari',
                           'outputs', 'insulin', 'assured', 'accent', 'mysimon', 'eleven', 'wives', 'ambient', 'mileage',
                           'oecd', 'adaptor', 'auburn', 'hyundai', 'pledge', 'vampire', 'relates', 'dice', 'merger',
                           'quad', 'dock', 'mods', 'nextel', 'framing', 'rwanda', 'sorts', 'vsnet', 'papua',
                           'hint', 'armor', 'riders', 'remark', 'dozens', 'varies', 'msie', 'picking', 'guards',
                           'buys', 'kruger', 'pockets', 'granny', 'pork', 'viral', 'inquire', 'pipes', 'laden',
                           'aruba', 'realtor', 'chassis', 'dubai', 'barn', 'pushing', 'fleece', 'fare', 'asus',
                           'pierce', 'sperm', 'bald', 'filme', 'craps', 'fuji', 'frost', 'mold', 'dame',
                           'yacht', 'prefers', 'alot', 'breach', 'whale', 'bedford', 'idle', 'mustang', 'wiring',
                           'pastor', 'shark', 'phases', 'grows', 'tract', 'ballet', 'bumper', 'webpage', 'garlic',
                           'hostels', 'senegal', 'banned', 'briefs', 'diffs', 'cove', 'mumbai', 'ozone', 'casa',
                           'radios', 'tariff', 'nvidia', 'pasta', 'muscles', 'serum', 'wrapped', 'swift', 'runtime', 
                            'inbox', 'focal', 'incl', 'samba', 'hostel', 'penguin', 'magical', 'miracle', 'reprint', 
                            'flex', 'yearly', 'wound', 'hash', 'hamburg', 'lazy', 'fathers', 'carb', 'lined', 'petite', 
                            'terrain', 'pens', 'strips', 'rangers', 'rotary', 'worm', 'boxed', 'cubic', 'deaf', 'kinase', 
                            'skirts', 'mats', 'labeled', 'marking', 'serbia', 'sheriff', 'griffin', 'guyana', 'spies', 
                            'blah', 'mime', 'deadly', 'feof', 'chevy', 'rounds', 'longest', 'tions', 'usda', 'keen', 
                            'flyer', 'peas', 'dosage', 'urls', 'baking', 'baths', 'brakes', 'nirvana', 'owns', 'sticky', 
                            'madness', 'emacs', 'blowing', 'heated', 'sparc', 'cardiac', 'dover', 'vatican', 'brutal', 
                            'token', 'zinc', 'seekers', 'guru', 'yields', 'levy', 'suited', 'numeric', 'skating', 'kinda',
                            'emperor', 'grad', 'bras', 'belts', 'blacks', 'rebates', 'burke', 'proudly', 'pulling', 
                            'obesity', 'curves', 'touring', 'vertex', 'tomato', 'andorra', 'expired', 'travels', 'waiver',
                            'pale', 'hayes', 'garcia', 'counted', 'declare', 'johns', 'valves', 'gaps', 'donors', 
                            'teaches', 'bufing', 'tragedy', 'dryer', 'painful', 'ruled', 'nato', 'prayers', 'funky', 
                            'joins', 'scary', 'mpegs', 'brunei', 'banana', 'slovak', 'cakes', 'idol', 'mixer', 'sbjct', 
                            'tooth', 'stays', 'affair', 'drove', 'washer', 'mines', 'rebound', 'fought', 'baghdad', 
                            'metres', 'pencil', 'titled', 'sphere', 'moss', 'ratios', 'concord', 'walnut', 'ladder', 
                            'italia', 'liberia', 'cork', 'hansen', 'workout', 'mali', 'colon', 'lanes', 'purse', 
                            'stating', 'dome', 'crest', 'triumph', 'welding', 'heel', 'alloy', 'condos', 'plots', 
                            'gently', 'tulsa', 'locking', 'draws', 'fridge', 'blanket', 'bloom', 'fraser', 'blades', 
                            'loops', 'surge', 'trauma', 'tahoe', 'advert', 'subaru', 'vanilla', 'picnic', 'souls', 
                            'spank', 'dumb', 'hollow', 'groove', 'pursuit', 'wires', 'mails', 'backing', 'sleeps', 
                            'endless', 'figured', 'orbit', 'niger', 'bacon', 'heater', 'cannon', 'circus', 'forbes', 
                            'moldova', 'paxil', 'spine', 'trout', 'feat', 'ntsc', 'cooked', 'apnic', 'fatty', 'pressed', 
                            'scanned', 'hunger', 'usps', 'surgeon', 'cement', 'missile', 'closes', 'conf', 'assists', 
                            'auditor', 'violin', 'prophet', 'bracket', 'oxide', 'oaks', 'naples', 'modems', 'harmful', 
                            'prozac', 'newark', 'paso', 'glucose', 'phantom', 'norm', 'turtle', 'warned', 'neural', 
                            'ware', 'fossil', 'badly', 'apollo', 'persian', 'greene', 'robots', 'grenada', 'foul', 
                            'keno', 'earning', 'mailman', 'sanyo', 'nested', 'somalia', 'movers', 'verbal', 'seas', 
                            'novelty', 'tiles', 'voyuer', 'tamil', 'garmin', 'fuzzy', 'grams', 'mrna', 'budgets', 
                            'toolkit', 'goat', 'erotica', 'forge', 'dense', 'brave', 'awful', 'impose', 'sega', 
                            'viewers', 'cdna', 'meyer', 'enters', 'savage', 'resumes', 'gage', 'existed', 'wagon', 
                            'favour', 'urge', 'smtp', 'peers', 'optimum', 'neon', 'quilt', 'mounts', 'refresh', 
                            'webcast', 'subtle', 'notre', 'stripes', 'cope', 'cradle', 'mambo', 'lime', 'flour', 
                            'bool', 'choir', 'blond', 'expects', 'jumping', 'fabrics', 'polymer', 'hygiene', 'poultry', 
                            'virtue', 'bouquet', 'mandate', 'spas', 'corpus', 'fibre', 'shades', 'jets', 'indices', 
                            'adware', 'intl', 'zoloft', 'halifax', 'ultram', 'cursor', 'donated', 'stuffed', 'insects',
                            'crude', 'maiden', 'viking', 'bored', 'cleanup', 'yarn', 'bother', 'bhutan', 'mating', 
                            'redhead', 'arrives', 'tractor', 'allah', 'unwrap', 'fares', 'hoped', 'pike', 'safer', 
                            'wagner', 'touched', 'cologne', 'gzip', 'wishing', 'ranger', 'newman', 'marsh', 'ctrl', 
                            'scared', 'theta', 'bent', 'laos', 'asylum', 'stake', 'outlets', 'arbor', 'poison')
