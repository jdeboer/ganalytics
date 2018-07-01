2018-06-23 Version type dimensions, e.g. ga:appVersion, are now coerced to `numeric_version` class, so that version numbers (e.g. ‘2.4.7’, ‘2.5.13’, ‘2.32.1’, etc...) can be correctly sorted and compared as if they were numeric values. Updated gademo.R. The dateRange class now inherits 'lubridate' interval as its superclass. It is now possible to query more than 10 metrics with just one query.

2018-05-30 Additional methods to coerce ganalytics segment classes and table filters for use with the 'googleAnalyticsR' package. Dynamic Segments objects now have a name property. Updated `googleAnalyticsR-demo` and examples in the readme file.

2018-02-18 Added methods for coercing a range of ganalytics classes into 'googleAnalyticsR' classes, so that ganalytics segments, filters and expressions can be used by the google_analytics function of the 'googleAnalyticsR' package.

2018-02-12 Scope and negation of segment conditions can now be defined at the filter level. Fixed bug where backslashes were being escaped incorrectly in expression operands. The methods of the Segment generic function are now split into two generic functions, Segment and Segments. Segments is used to set or get a named list segments, whereas Segment is for defining a single segment to be added to a Segments list.

2017-12-27 Support for %starts_with% and %ends_with% operators in addition to %contains% which was incorrectly described as %starts_with% previously. Added PerProduct method for setting metric scope for dynamic segmentation. ganalytics now searches the working directory for a json file containing the API client ID and secret supplied by Google Cloud Console credentials manager.

2015-12-19 Username (email address) for Google API authentication can now be set using via a system environment variable with a key named as [APPNAME]_USER, where [APPNAME] is 'GOOGLE_APIS' default, e.g. GOOGLE_APIS_USER = me@example-domain.com
Updates to support recent API additions, including Management API view bot filtering flag.

2015-12-12 Ability to query past 10 metrics by automatically joining API results by there dimensions. Added methods for NOTing one-of-in-list '[]' dimension and within-range '<>' metric expressions. Default date range has been pushed back 1 day to ensure complete data is returned by MCF queries.

2015-11-27 Support multiple segments being applied to a query.

2015-11-25 profileId argument for Query generators is deprecated.

2015-10-06 [] and [[]] operators now supported by Management API collection objects, so get a single entity resource from collection such as Accounts, Properties, Views, Goals, etc.

2015-09-25 gaGoal objects now include goal configuration details.

2015-09-20 Comparators are now generic functions with methods for supplying a .var (LHS) and .operand (RHS)

2015-09-18 PerUser and PerSession can now be used instead of SegmentFilters to create a scoped segment filter list. Also, Include and Exclude have been added to add include and exclude (i.e negate) filters to a segment definition, rather than needing to use the negate argument of the Sequence and SegmentConditionFilter functions.
Added function for generating a segment definition from a list where ... can be used to mean 'followed-by / Later' prior to the next step in the sequence. Note this function uses non-standard evaluation.
PerHit can be used to transform a condition filter into a sequence of length one, which offers a powerful form of segmentation where all conditions must be met for a single hit rather than scoped across sessions or users.
Expr can now be used with a formula denoted by the prefix `~`. This uses non-standard evaluation so that variable names and condition operators do not need to be surrounded by quotation marks.
Added IsNegated generic function and method for testing whether a segment filter's negated slot is set to TRUE.

2015-09-16 Renamed segmentation functions: GaSequenceCondition -> Sequence; GaNonSequenceCondition -> SegmentConditionFilter; GaSegmentCondition -> SegmentFilters

2015-09-15 Renamed GaSequence to Sequence. Added PerUser, PerSession and PerHit generic functions for setting the scope of segment filters, and metrics conditions used within segments. Also, renamed GaScopeLevel and GaScopeLevel<- functions to ScopeLevel and ScopeLevel<- respectively. Sampling warnings are now more informative by notifying you of the total sample size and space with a sampling rate percentage too. Authentication credentials are remembered between commands without the need for the user to store them in a local variable.

2015-09-01 Added Domain Specific Language (DSL) functions utilising Non-standard Evaluation (NSE) for defining conditions and sequences.
2015-08-20 Renamed segmentation functions: GaStartsWith -> First, GaPreceeds -> Later, GaImmediatelyPreceeds -> Then . Renamed operator to comparator. Added functions to set scope of segment filters and segment metric expressions.

2015-08-17 Update to latest dimension and metrics metadata and added support for custom dimensions and device category as view filter fields. Changed default metric for real-time queries to rt:pageviews.
2015-08-14 Added demos. Added support for new alphanumeric segment IDs. Foundations to support multiple segments within a single query.
2015-06-05 Support the use of a 'lubridate' interval object as a dateRange object for GA Reporting API queries.

2015-05-04 Added support for real-time and multi-channel-funnels reporting APIs - both formulating queries and processing the query responses.

2015-05-02 Renaming of many functions by removing the Ga prefix in the name, with backwards compatibility for the old function names via aliases.

2015-04-26 Support for base R logical expression operators for defining GA query expressions. Added validity check for dimension and metric names of MCF and RT expressions.

2015-04-06 Added function for setting or getting the scope level of segments and expressions.
2015-04-05 Added support for dateOfSession dimension when used for segmentation. Suggest valid dimension and metric names to the user if a partial match is found. Automatic handling of date formatting for API requests and responses.
2015-04-04 Added support for list and range comparator operators when used for segment expressions.

2015-03-22 Ability to update and delete existing resource entities such as user links.
2015-03-21 Query view filter definitions. Ability to insert new resource entities where supported by the Management API, e.g. adding new user links. Also, ability to query definitions of custom dimensions and metrics via the management API
2015-02-12 Ability to query user permissions for accounts, properties and views via the Management API.
2015-02-09 Added Google Tag Manager classes and methods.

2015-01-29 Extend GaSegment methods to accept a gaUserSegment class object in addition to already accepted expressions and segment IDs.

2015-01-26 Automatically select view from a given gaProperty or gaAccount class object. Extend GaQuery methods to accept a gaView class object in addition to already accepted view IDs. Ability to request user defined segments via the Management API. If no OAuth app creds provided, then use JSON file in current directory called ".app_oauth_creds.json" if exists.

2015-01-25 Query the Google Tag Manager API
2015-01-12 Values for various class properties defined as factors with appropriate levels.

2015-01-11 Ability to automatically select the default view of a given property
2014-12-27 Functions to retrieve details about available Google Analytics accounts, properties and views that can be queried.

2014-12-20 Implemented exponential back off algorithm to improve reliability of fetching reporting API data in case of intermittent network outages.
2014-11-22 Optionally supply a username to use for the OAuth2.0 user authentication dance with Google.
2014-11-21 Optionally supply a JSON file from the Google APIs console that contains the client ID and secret to use for OAuth2.0 authentication.

2014-09-30 Ability to negate a segment expression using R's NOT (!) operator.

2014-09-16 Include sample size and sample space as attributes in the returned dataframe from a reporting API request.
2014-08-09 Support for defining unified segment expressions. Added optional argument to set the sampling level of a query.

2014-06-21 Query from multiple views with a binded response

2014-06-05 Upgrade to using Meta Data API for updating available dimensions and metrics
2014-06-04 Warning given for queries resulting in a sampled report being returned

2014-05-23 Upgrade to OAuth2.0 functionality built into httr
2013-09-16 Implemented function to split a date range into N or daily increments
2013-06-10 Automate update of available dimensions and metrics. Abstraction of Google APIs request as a generalised function
2013-05-31 Implemented OAuth2.0 reference classes

2013-05-25 Initial version released via GitHub
