//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA,  2005, 2006, 2007, 2008.
//
// Some common SQL defines and structs
//
#ifndef __SQLCOMMON__
#define __SQLCOMMON__
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <dirent.h>
#include <stdio.h>
#include <list>
#include <string>
#include <time.h>
#include <map>
#include <mysql/mysql.h>
#include <string.h>
#include <errno.h>
#include "packfile.hh"

using namespace std;
#define ACTION_REBOOT     0x00000001
#define ACTION_RESTART    0x00000002
#define ACTION_RELOAD_LIB 0x00000004
#define ACTION_RELOAD_M1  0x00000008

typedef list<string> CStringList;
typedef list<string>::iterator CStringListIterator;

typedef list<int> CIntList;
typedef list<int>::iterator CIntListIterator;
#define DB_CONT_REGULAR 1
#define DB_CONT_HARDLINK 2
#define DB_CONT_SYMLINK 3
#define DB_CONT_DIRECTORY 4
#define DB_CONT_BLOCKDEV 5
#define DB_CONT_CHARDEV 6
#define DB_CONT_FIFO 7
#define DB_CONT_UNIXSOCK 8
#define DB_CONT_PACKFILE 9



struct CAccount {
    CAccount(long aDbID = 0,  
	     string aEmail = "",   
	     string aPassword = "",  
	     string aPasswdHint = "",
	     string aFirstName = "",
	     string aMiddleName = "",
	     string aLastName = "",
	     string aAddress1 = "",
	     string aAddress2 = "",
	     string aCity = "",
	     string aZip = "",
	     string aState = "",
	     string aCountry = "",
	     string aHomePhone = "",
	     string aWorkPhone = "",
	     string aCellPhone = "",
	     string aComment = "",
	     bool aFreeShipping = false,
	     bool aTaxExemp = false
	     ):
	mDbID(aDbID),
	mEmail(aEmail),   
	mPassword(aPassword),  
	mPasswdHint(aPasswdHint),
	mFirstName(aFirstName),
	mMiddleName(aMiddleName),
	mLastName(aLastName),
	mAddress1(aAddress1),
	mAddress2(aAddress2),
	mCity(aCity),
	mZip(aZip),
	mState(aState),
	mCountry(aCountry),
	mHomePhone(aHomePhone),
	mWorkPhone(aWorkPhone),
	mCellPhone(aCellPhone),
	mComment(aComment),
	mFreeShipping(aFreeShipping),
	mTaxExempt(aTaxExemp) {}

    long mDbID;
    string mEmail;   
    string mPassword;  
    string mPasswdHint;
    string mFirstName;
    string mMiddleName;
    string mLastName;
    string mAddress1;
    string mAddress2;
    string mCity;
    string mZip;
    string mState;
    string mCountry;
    string mHomePhone;
    string mWorkPhone;
    string mCellPhone;
    string mComment;
    bool mFreeShipping;
    bool mTaxExempt;
};

typedef list<CAccount> CAccountList;
typedef list<CAccount>::iterator CAccountListIterator;


struct CM1Unit {
    CM1Unit(long aDbID = 0L,  
	    string aSerial = "",
	    long aBuildOrderID = 0L,
	    long aDeviceKeyID = 0L,
	    string aDiskID = "",
	    long aOwnerAccountID = 0L,
	    time_t aManufactureDate = 0L):

	mDbID(aDbID),
	mSerial(aSerial),   
	mBuildOrderID(aBuildOrderID),  
	mDeviceKeyID(aDeviceKeyID),
	mDiskID(aDiskID),
	mOwnerAccountID(aOwnerAccountID),
	mManufactureDate(aManufactureDate)
    {}
    long mDbID;
    string mSerial;
    long mBuildOrderID;
    long mDeviceKeyID;
    string mDiskID;
    long mOwnerAccountID;
    time_t mManufactureDate;
};

typedef list<CM1Unit> CM1UnitList;
typedef list<CM1Unit>::iterator CM1UnitListIterator;


struct CDependency {
    CDependency(long aVersionID,   // pf_version.id we are dependent on
		string aEmail = "",      // Email of account creating packfile we are dependent onn
		string aName = "",      // Name of dependency packfile
		long aMajor = 0,
		long aMinor = 0,
		long aPatch = 0):
	mVersionID(aVersionID),
	mEmail(aEmail),
	mName(aName),
	mMajor(aMajor),
	mMinor(aMinor),
	mPatch(aPatch) {}
	
    long mVersionID;
    string mEmail;
    string mName;
    long mMajor;
    long mMinor;
    long mPatch;
};
typedef list<CDependency> CDependencyList;
typedef list<CDependency>::iterator CDependencyListIterator;

struct CPackfile {
    CPackfile(long aID,
	      long aSVNRevision,
	      string aName,
	      string aRestartAction,
	      long aCreatorAccountID,

	      long aVersionID,
	      long aMajor,
	      long aMinor,
	      long aPatch,
	      time_t aCreatorTimeStamp):
	mID(aID),
	mSVNRevision(aSVNRevision),
	mName(aName),
	mRestartAction(aRestartAction),
	mCreatorAccountID(aCreatorAccountID),

	mVersionID(aVersionID),
	mMajor(aMajor),
	mMinor(aMinor),
	mPatch(aPatch),
	mCreatorTimeStamp(aCreatorTimeStamp) {}
	
    long mID;
    long mSVNRevision;
    string mName;
    string mRestartAction;
    long mCreatorAccountID;

    long mVersionID;
    long mMajor;
    long mMinor;
    long mPatch;
    time_t mCreatorTimeStamp;
};
typedef list<CPackfile> CPackfileList;
typedef list<CPackfile>::iterator CPackfileListIterator;

struct CContent {
    CContent(const string &aPath, 
	     size_t aSize, 
	     mode_t aPermission, 
	     int aType,
	     string aSymlinkTarget = "",
	     unsigned long aMajor = 0,
	     unsigned long aMinor = 0,
	     class CContent *aLink = 0, // Hardlink to this file
             long aDbID = 0,
	     long aLinkID = 0):
	mPath(aPath),
	mSize(aSize),
	mPermission(aPermission),
	mType(aType),
	mSymlinkTarget(aSymlinkTarget),
	mMajor(aMajor),
	mMinor(aMinor),
	mDbID(aDbID),
	mLink(aLink),
	mLinkID(aLinkID),
	mStart(0L) {}
    string mPath;
    size_t mSize; // 0 == directory.
    mode_t mPermission; 
    int mType; // See DB_CONT_*
    string mSymlinkTarget; // Target of synbolic link
    unsigned long mMajor;
    unsigned long mMinor;
    unsigned long mDbID; // pf_component.id assigned to this entry when stored. Used by hardlink db xref.
    long mLinkID; // Unresolved pf_component.id reference to file this entry is hardlinked to.
    CContent *mLink; // ResolvedHardlink to this entry. 
    size_t mStart; // Used to mark start point in larger file. ugly
};


//
// MySQL escape_string support functions to make life
// somewhat easier.
//
struct CStringContext: public list<char *> {
    ~CStringContext(void) {
	while(begin() != end()) {
	    free(back());
	    pop_back();
	}	
    }
};

extern char *ctx_strcpy(const char *aString, int aLength, CStringContext *aContext);

extern char *ctx_escape_mysql(MYSQL *aDbDesc, 
		       const char *aString, 
		       int aLength, 
		       CStringContext *aContext);

extern char *ctx_sprintf(CStringContext *aContext, char *aFormat, ...);

// CEscString alloc_total_context_length(int aAdditionalLength, CEscStringContext *aContext);

//
// Store an encryption key data.
//
struct CKeyData {
    CKeyData(void): 
	mKeyName(""),
	mPubKeyData(0),
	mPubKeyDataLength(0),
	mPrivKeyData(0),
	mPrivKeyDataLength(0),
	mBinKeyData(0),
	mBinKeyDataLength(0),
	mDeviceKeyFlag(false) {
    }

    CKeyData(const CKeyData &aFrom): 
	mKeyName(aFrom.mKeyName),
	mPubKeyData((char *) memcpy(new char[aFrom.mPubKeyDataLength], aFrom.mPubKeyData, aFrom.mPubKeyDataLength)),
	mPubKeyDataLength(aFrom.mPubKeyDataLength),
	mPrivKeyData((char *) memcpy(new char[aFrom.mPrivKeyDataLength], aFrom.mPrivKeyData, aFrom.mPrivKeyDataLength)),
	mPrivKeyDataLength(aFrom.mPrivKeyDataLength),
	mBinKeyData((char *) memcpy(new char[aFrom.mBinKeyDataLength], aFrom.mBinKeyData, aFrom.mBinKeyDataLength)),
	mBinKeyDataLength(aFrom.mBinKeyDataLength),
	mDeviceKeyFlag(aFrom.mDeviceKeyFlag) {
    }

    CKeyData(string aKeyName, 
	     char *aPrivKeyData, 
	     int aPrivKeyDataLength, 
	     char *aPubKeyData, 
	     int aPubKeyDataLength, 
	     char *aBinKeyData, 
	     int aBinKeyDataLength, 
	     bool aDeviceKeyFlag): 
	mKeyName(aKeyName),
	mPubKeyData(0),
	mPubKeyDataLength(aPubKeyDataLength),
	mPrivKeyData(0),
	mPrivKeyDataLength(aPrivKeyDataLength),
	mBinKeyData(0),
	mBinKeyDataLength(aBinKeyDataLength),
	mDeviceKeyFlag(aDeviceKeyFlag) {

	if (aPubKeyData && aPubKeyDataLength) {
	    mPubKeyData = new char[aPubKeyDataLength];
	    memcpy(mPubKeyData, aPubKeyData, mPubKeyDataLength);
	}

	if (aPrivKeyData && aPrivKeyDataLength) {
	    mPrivKeyData = new char[aPrivKeyDataLength];
	    memcpy(mPrivKeyData, aPrivKeyData, mPrivKeyDataLength);
	}

	if (aBinKeyData && aBinKeyDataLength) {
	    mBinKeyData = new char[aBinKeyDataLength];
	    memcpy(mBinKeyData, aBinKeyData, mBinKeyDataLength);
	}
    }

    ~CKeyData() {
	if (mPubKeyData)
	    delete[] mPubKeyData;

	if (mPrivKeyData)
	    delete[] mPrivKeyData;

	if (mBinKeyData)
	    delete[] mBinKeyData;
    }

    CKeyData &operator=(const CKeyData &aFrom) { 
	if (&aFrom == this)
	    return *this;

	mKeyName = aFrom.mKeyName;
	mPubKeyData = (char *) memcpy(new char[aFrom.mPubKeyDataLength], aFrom.mPubKeyData, aFrom.mPubKeyDataLength);
	mPubKeyDataLength = aFrom.mPubKeyDataLength;
	mPrivKeyData = (char *) memcpy(new char[aFrom.mPrivKeyDataLength], aFrom.mPrivKeyData, aFrom.mPrivKeyDataLength);
	mPrivKeyDataLength = aFrom.mPrivKeyDataLength;
	mBinKeyData = (char *) memcpy(new char[aFrom.mBinKeyDataLength], aFrom.mBinKeyData, aFrom.mBinKeyDataLength);
	mBinKeyDataLength = aFrom.mBinKeyDataLength;
	mDeviceKeyFlag = aFrom.mDeviceKeyFlag;
	return *this;
    }



    void setPubKeyData(char *aPubKeyData, int aPubKeyDataLength) {
	if (mPubKeyData)
	    delete[] mPubKeyData;

	mPubKeyData = new char[aPubKeyDataLength];
	mPubKeyDataLength = aPubKeyDataLength;
	memcpy(mPubKeyData, aPubKeyData, mPubKeyDataLength);
    }

    void setPrivKeyData(char *aPrivKeyData, int aPrivKeyDataLength) {
	if (mPrivKeyData)
	    delete[] mPrivKeyData;

	mPrivKeyData = new char[aPrivKeyDataLength];
	mPrivKeyDataLength = aPrivKeyDataLength;
	memcpy(mPrivKeyData, aPrivKeyData, mPrivKeyDataLength);
    }

    void setBinKeyData(char *aBinKeyData, int aBinKeyDataLength) {
	if (mBinKeyData)
	    delete[] mBinKeyData;

	mBinKeyData = new char[aBinKeyDataLength];
	mBinKeyDataLength = aBinKeyDataLength;
	memcpy(mBinKeyData, aBinKeyData, mBinKeyDataLength);
    }

    string mKeyName;
    char *mPubKeyData;
    int mPubKeyDataLength;
    char *mPrivKeyData;
    int mPrivKeyDataLength;
    char *mBinKeyData;
    int mBinKeyDataLength;
    bool mDeviceKeyFlag;
};


typedef list<CKeyData> CKeyDataList;
typedef list<CKeyData>::iterator CKeyDataListIterator;



#define DB_ALL -1

#define DB_STATUS_PENDING 1
#define DB_STATUS_ACTIVE 2
#define DB_STATUS_COMPLETE 3
#define DB_STATUS_CANCELLED 4

struct CBuildOrder {
    CBuildOrder(long aID,
		long aPartID,
		time_t aCreated,
		int aQuantity,
		int  aRemaining,
		string aDescription,
		int aStatus):
	mID(aID),
	mPartID(aPartID),
	mCreated(aCreated),
	mQuantity(aQuantity), 
	mRemaining(aRemaining), 
	mDescription(aDescription),
	mStatus(aStatus) {} 

    CBuildOrder(void):
	mID(0),
	mPartID(0),
	mCreated(0),
	mQuantity(0), 
	mRemaining(0),  // Not calculated
	mDescription(""),
	mStatus(-1) {}  // Unknown
    long mID;
    long mPartID; // Part number for this build order.
    time_t mCreated;
    int mQuantity; // How many units does the build order specify
    int mRemaining; // How many are left to assemble.
    string mDescription;
    int mStatus; // DB_STATUS_XX
};

typedef list<CBuildOrder> CBuildOrderList;
typedef list<CBuildOrder>::iterator CBuildOrderListIterator;


struct CLot {
    CLot(long aID,
	 long aPartID,
	 time_t aCreated,
	 int aQuantity,
	 int aRemaining,
	 string aPO, 
	 string aDescription,
	 int aStatus):
	mID(aID),
	mPartID(aPartID),
	mCreated(aCreated),
	mQuantity(aQuantity), 
	mRemaining(aRemaining), 
	mPO(aPO),
	mDescription(aDescription),
	mStatus(aStatus) {} 

    CLot(void):
	mID(0),
	mPartID(0L),
	mCreated(0),
	mQuantity(0), 
	mRemaining(0), 
	mPO(""),
	mDescription(""),
	mStatus(-1) {} 
    long mID;
    long mPartID; // Part number for this lot
    time_t mCreated; // When it was created.
    long mQuantity; // How many items was in this lot originally.
    long mRemaining; // How manyu remains
    string mPO; // Purchase order. Links to quickbooks.
    string mDescription;
    int mStatus; // DB_STATUS_XX
};

typedef list<CLot> CLotList;
typedef list<CLot>::iterator CLotListIterator;


struct CPart {
    CPart(long aID,
	  string aPartNr,
	  string aDescription, 
	  int aQuantity):
	mID(aID),
	mPartNr(aPartNr),
	mDescription(aDescription),
	mQuantity(aQuantity) { }

    CPart(void):
	mID(0),
	mPartNr(""),
	mDescription(""),
	mQuantity(0) { }

    long mID;
    string mPartNr; // Part number - Ties int quickbooks.
    string mDescription; // Description of part
    int mQuantity; // Temporary storage value  for quantity.
};

typedef list<CPart> CPartList;
typedef list<CPart>::iterator CPartListIterator;



struct CSalesOrder {
    CSalesOrder(long aID,
		time_t aCreated,
		long aAccountID,
		string aTransNr,
		string aPartNr,
		int aQuantity,
		float aPrice,
		float aTax,
		float aShippingFee):
	mID(aID),
	mCreated(aCreated),
	mAccountID(aAccountID),
	mTransNr(aTransNr),
	mPartNr(aPartNr),
	mQuantity(aQuantity),
	mPrice(aPrice),
	mTax(aTax),
	mShippingFee(aShippingFee) { }

    CSalesOrder(void):
	mID(0),
	mCreated(0),
	mAccountID(0),
	mTransNr(""),
	mPartNr(""),
	mQuantity(0),
	mPrice(0.0),
	mTax(0.0),
	mShippingFee(0.0) { }


    long mID;
    time_t mCreated;
    long mAccountID;
    string mTransNr;
    string mPartNr;
    int mQuantity;
    float mPrice;
    float mTax;
    float mShippingFee;
};

typedef list<CSalesOrder> CSalesOrderList;
typedef list<CSalesOrder>::iterator CSalesOrderListIterator;


typedef list<long> CLongList;
typedef list<long>::iterator CLongListIterator;

typedef list<CContent> CContentList;
typedef list<CContent>::iterator CContentListIterator;

typedef map<ino_t ,CContent *> CInodeMap;
typedef map<ino_t ,CContent *>::iterator CInodeMapIterator;

typedef map<long ,CContent *> CIDMap;
typedef map<long ,CContent *>::iterator CIDMapIterator;

// func defs
unsigned long time_stamp(void);

extern bool extract_packet_id(string &aID, 
			      string &aAccount, 
			      string &aProvider, 
			      string &aPacket,
			      int &aMajor,
			      int &aMinor,
			      int &aPatch);

extern int db_connect(MYSQL **aDbDesc, string aHost, string aUser, string aPasswd, string aDatabase, string aCharSet = "utf8" );
extern long db_find_account_by_id(MYSQL *aDbDesc, long aAccountID, CAccount *aResult);
extern int db_find_account_by_email(MYSQL *aDbDesc, string aEmail, CAccount *aResult);
extern int  db_find_packfile(MYSQL *aDbDesc, string aPacketName, CPackfileList &lst);
extern long db_create_packfile(MYSQL *aDbDesc, CPackfile &aPackfile);
extern bool db_delete_packfile(MYSQL *aDbDesc, CPackfile &aPackFile);

extern long db_add_file(MYSQL *aDbDesc, long aVersionID, CContent &aContent, string aRootPrefix, int aPartSize);

extern int db_get_files(MYSQL *aDbDesc, long aVersionID, CContentList &aResult);
extern int db_get_file_content(MYSQL *aDbDesc, CContent *aContent, char *aResult);

extern long db_add_dependency(MYSQL *aDbDesc, long aPackfileVersionID, long aNeededVersionID);
extern int db_get_dependencies(MYSQL *aDbDesc, long aPacketVersionID, CDependencyList *aResult);

extern long db_get_serial(MYSQL *aDbDesc, string aSerial, long &aDbID, time_t &aAssignedTS);

// Set aAssigned to 0 for assigned=NULL in db.
// Batch is the serial number batch. Inncrease by one for each new 
// batch of seraial numbers added to table
extern int db_add_serial(MYSQL *aDbDesc,  string aSerial, time_t aAssignedTS, int aBatch);
extern int db_get_max_serial_batch(MYSQL *aDbDesc, int &aBatch);
extern long db_assign_next_serial(MYSQL *aDbDesc, string &aDbID);
extern long db_unassign_serial(MYSQL *aDbDesc, long aDbID);


extern long db_get_m1_unit(MYSQL *aDbDesc, 
			   string aSerial, // Serial number of unit to retrieve
			   CM1Unit &aUnit);

extern int db_add_m1_unit(MYSQL *aDbDesc, 
			  string aPartNr,
			  string aSerialNr,
			  char *aDevicePrivKey,
			  int aDevicePrivKeyLength,
			  char *aDevicePubKey,
			  int aDevicePubKeyLength,
			  char *aDeviceBinKey,
			  int aDeviceBinKeyLength,
			  string aDiskSerialNr,
			  long aOwnerAccountID); // Ref to account


// Return multiple keys
extern long db_find_encryption_keys(MYSQL *aDbDesc, 
				    CKeyDataList &aResult,
				    string aKeyName = "", // Wildcard search %ABC%
				    bool aDeviceKeyFlag = false);  // Device flag set or not


// Return trusted.enc_key.id
extern long db_get_encryption_key(MYSQL *aDbDesc, 
				  string aKeyName, // Symbolic name of key 
				  CKeyData &aKeyData);

extern long db_add_encryption_key(MYSQL *aDbDesc, 
				  string aKeyName,
				  bool aDeviceKeyFlag, 
				  char *aPrivKeyData,
				  int aPrivKeyDataLength,
				  char *aPubKeyData,
				  int aPubKeyDataLength,
				  char *aBinKeyData,
				  int aBinKeyDataLength);

extern int db_add_account(MYSQL *aTrustedDesc, 
			  MYSQL *aUntrustedDesc, 
			  string aEmail,
			  string aPassword,
			  string aPasswordHint,
			  string aFirstName,
			  string aMiddleName,
			  string aLastName,
			  string aAddress1,
			  string aAddress2,
			  string aCity,
			  string aZip,
			  string aState,
			  string aCountry,
			  string aHomePhone,
			  string aWorkPhone,
			  string aCellPhone,
			  string aComment,
			  FILE *aLog);


extern int db_edit_account(MYSQL *aTrustedDesc, 
			   MYSQL *aUntrustedDesc,
			   FILE *aLog, 
			   string aAccountEmail,
			   string aEmail, bool aUpdateEmail,
			   string aPassword, bool aUpdatePassword,
			   string aPasswordHint, bool aUpdatePasswordHint,
			   string aFirstName, bool aUpdateFirstName,
			   string aMiddleName, bool aUpdateMiddleName,
			   string aLastName, bool aUpdateLastName,
			   string aAddress1, bool aUpdateAddress1,
			   string aAddress2, bool aUpdateAddress2,
			   string aCity, bool aUpdateCity,
			   string aZip, bool aUpdateZip,
			   string aState, bool aUpdateState,
			   string aCountry, bool aUpdateCountry,
			   string aHomePhone, bool aUpdateHomePhone,
			   string aWorkPhone, bool aUpdateWorkPhone,
			   string aCellPhone, bool aUpdateCellPhone,
			   string aComment, bool aUpdateComment,
			   CM1UnitList &aOwnedUnits);



long db_create_lot(MYSQL *aDBDesc,
 		   int aPartID, // Part # of lot.
 		   time_t aCreated,  // When was lot created
 		   int aQuantity, // Quantity of aPartID in lot. remaining col will be set to this value.
 		   string aPO, // Purchase order string. Ties into QB PO
 		   string aDescription, // Description of lot.
 		   int aStatus); 

long db_create_build_order(MYSQL *aDBDesc, 
			   CPart *aPart,
			   time_t aCreated,
			   int aStatus,
			   string aDescription,
			   int aQuantity);

extern long db_create_part(MYSQL *aDBDesc,
			   string aPartNr,   // Textual part nr.
			   string aDescription); // Description of lot.

extern int db_get_all_parts(MYSQL *aDbDesc,
			    CPartList &aResult);

extern long db_get_part(MYSQL *aDbDesc,
			long aPartID,
			CPart &aResult);

extern long db_get_part_by_part_nr(MYSQL *aDbDesc,
				   string aPartNr,
				   CPart &aResult);

int db_get_bom_by_id(MYSQL *aDBDesc, 
		     long aPartID,
		     CPartList &aResult);

int db_get_build_order(MYSQL *aDbDesc, 
		       long aPartID, // -1 == all part ids
		       int aStatus,  // -1 == all statuses
		       CBuildOrderList &aResult);

extern long db_get_lots(MYSQL *aDbDesc,
			long aPartID, // -1 == all parts
			int aStatus, // -1 == all status
			CLotList &aResult);

extern int db_deplete_lot(MYSQL *aDBDesc, 
			  int aCount,
			  CLot &aLot);

extern bool db_deplete_build_order(MYSQL *aDBDesc, 
				   CBuildOrder *aBuildOrder, // The build order to 
				   int aCount); // Number of units to deplete.

extern bool db_deactivate_lot(MYSQL *aDBDesc, 
			      long aLotID);

extern int db_setup_build_order_lots(MYSQL *aDBDesc, 
				     long aPartID, // Part to setup assembly lots for
				     CLotList &aActiveLots); // All active lots

extern bool db_activate_next_lot(MYSQL *aDBDesc, long aPartID, CLot &aNewActiveLot);

extern bool db_deactivate_build_order(MYSQL *aDBDesc, 
				      long aBuildOrderID);
    
extern bool db_activate_next_build_order(MYSQL *aDBDesc, 
					 long aPartID);

extern long db_create_sales_order(MYSQL *aDBDesc,
				  time_t aCreated,
				  string aTransNr,
				  string aOrderID,
				  CAccount *aAccount,  
				  CPart *aPart,
				  int aQuantity,
				  float aPrice,
				  float aShippingFee,
				  float aTax);

extern int db_get_sale_order(MYSQL *aDBDesc, 
			     CSalesOrderList &aResult);

extern bool db_get_discount(MYSQL *aDBDesc, 
			    long aAccountID,
			    string aPartNr,
			    int &aResultStartVolume,
			    float &aResultPercent,
			    float &aResultPrice);

#endif // __SQLCOMMON__
