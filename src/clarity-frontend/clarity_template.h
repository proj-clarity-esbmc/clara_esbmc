/*
  The template/library for Clarity built-in variables, function and data structure
*/

/* NOT SURE HOW THIS FILE IS CONSUMED */

#include <nlohmann/json.hpp>
#include <unordered_set>

#ifndef clarity_TEMPLATE_H_
#  define clarity_TEMPLATE_H_

namespace ClarityTemplate
{
/// header & typedef

const std::string clar_header = R"(
#include <stddef.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

)";

/*
  int = int128_t
  uint == uint128_t

	address has two types, standard and contract
	standard can be decomposed to 1 byte version, 20 bytes public key hash
	contract is standard + 1-40 ascii characters
	see https://docs.stacks.co/clarity/functions#principal-destruct
*/
const std::string clar_typedef = R"(
typedef signed _BitInt(128) int128_t;
typedef unsigned _BitInt(128) uint128_t;
typedef _Bool bool;


#define CLARITY_ADDRESS_TYPE_STANDARD 1
#define CLARITY_ADDRESS_TYPE_CONTRACT 2

typedef struct {
  int address_type;
	int address_version;
	char pubkey_hash[20];
	char contract_name[40];
} address_t;

typedef struct principal principal;
struct principal
{
    bool contract_is_principal;
    bool contract_is_standard;
    char issuer_principal_str[149];
  
};


)";

const std::string clar_optionals = R"(
#define TYPEDEF_OPTIONAL(type) typedef struct optional_##type optional_##type;

#define DEFINE_SOME(type) type some_##type (optional_##type x) \
{ \
	assert (!x.is_none); \
	return x.value;	\
}

#define DEFINE_OPTIONAL(type) struct optional_##type { bool is_none; type value; }


// optional string requires special handling
typedef struct optional_string optional_string;
typedef struct optional_buff	optional_buff;

struct optional_string {
	bool is_none;
	char *value;
};

struct optional_buff {
	bool is_none;
	unsigned char *value;
};



char* some_string(optional_string x)
{
	assert (!x.is_none);
	return x.value;
}


unsigned char* some_buff(optional_buff x)
{
	assert (!x.is_none);
	return x.value;
}


TYPEDEF_OPTIONAL(int128_t);
TYPEDEF_OPTIONAL(uint128_t);
TYPEDEF_OPTIONAL(bool);
TYPEDEF_OPTIONAL(principal);


DEFINE_OPTIONAL(int128_t);
DEFINE_OPTIONAL(uint128_t);
DEFINE_OPTIONAL(bool);
DEFINE_OPTIONAL(principal);


DEFINE_SOME(int128_t);
DEFINE_SOME(uint128_t);
DEFINE_SOME(bool);
DEFINE_SOME(principal);




)";

const std::string clar_lists = R"(

	#define TYPEDEF_LIST(type) typedef struct list_##type list_##type;

	#define DEFINE_LIST(type) struct list_##type { int size; type* items; }


	TYPEDEF_LIST(int128_t);
	TYPEDEF_LIST(uint128_t);
	TYPEDEF_LIST(bool);
	TYPEDEF_LIST(principal);

	DEFINE_LIST(int128_t);
	DEFINE_LIST(uint128_t);
	DEFINE_LIST(bool);
	DEFINE_LIST(principal);

)";

/// Variables
// the value of these variables need to be set to rand afterwards

#  if 0
TODO: not used
const std::string clar_msg = R"(
uint128_t msg_data;
address_t msg_address;
__uint32_t msg_sig;
uint128_t msg_value;
)";
#  endif

const std::string clar_tx = R"(
address_t tx_sender;
address_t contract_caller;
address_t tx_sponsorM;


)";

#  if 0
TODO: some of this is in https://docs.stacks.co/clarity/functions#get-block-info

const std::string clar_block = R"(
uint128_t block_basefee;
uint128_t block_chainid;
address_t block_coinbase;
uint128_t block_difficulty;
uint128_t block_gaslimit;
uint128_t block_number;
uint128_t block_prevrandao;
uint128_t block_timestamp;
)";
#  endif

// const std::string clar_vars = clar_msg + clar_tx + clar_block;
const std::string clar_vars = clar_tx;

/// functions
// if the function does not currently have an actual implement,
// leave the params empty.

// https://docs.soliditylang.org/en/latest/units-and-global-variables.html#special-variables-and-functions

const std::string blockhash = R"(
uint128_t blockhash();
)";

const std::string gasleft = R"(
uint128_t gasleft();
)";

const std::string clar_abi = R"(
uint128_t abi_encode();
uint128_t abi_encodePacked();
uint128_t abi_encodeWithSelector();
uint128_t abi_encodeWithSignature();
uint128_t abi_encodeCall();
)";

const std::string clar_math = R"(
uint128_t addmod(uint128_t x, uint128_t y, uint128_t k)
{
	return (x + y) % k;
}

uint128_t mulmod(uint128_t x, uint128_t y, uint128_t k)
{
	return (x * y) % k;
}

uint128_t keccak256();
uint128_t sha256();
address_t ripemd160();
address_t ecrecover();
)";

const std::string clar_string = R"(
char* string_concat(char *x, char *y)
{
	strcat(x, y);
	return x;
}


char* string_ascii_concat(char *s1, char *s2)
{
	int l1 = strlen(s1);
    int l2 = strlen(s2);
	
	// for this to work properly always pass --force-malloc-success
    char *out = malloc(l1 + l2 + 1);
	assert(out != NULL);
    memcpy(out,s1, l1);
    memcpy(out + l1,s2, l2);
	out[l1 + l2] = 0;
	return out;
}

char* concat(char *s1, char *s2)
{
	return string_ascii_concat(s1, s2);
}

// ml-[TODO] add other concat functions
)";

const std::string clar_byte = R"(
void byte_concat();
)";

const std::string clar_funcs =
  blockhash + gasleft + clar_abi + clar_math + clar_string + clar_byte;

/// data structure

/* https://github.com/rxi/map */
const std::string clar_mapping = R"(
#ifndef MAP_H
#define MAP_H

struct map_node_t;
typedef struct map_node_t map_node_t;

typedef struct
{
	map_node_t **buckets;
	unsigned nbuckets, nnodes;
} map_base_t;

typedef struct
{
	unsigned bucketidx;
	map_node_t *node;
} map_iter_t;

#define map_t(T)         \
	struct               \
	{                    \
		map_base_t base; \
		T *ref;          \
		T tmp;           \
	}

#define map_init(m) \
	memset(m, 0, sizeof(*(m)))

#define map_deinit(m) \
	map_deinit_(&(m)->base)

#define map_get(m, key) \
	((m)->ref = map_get_(&(m)->base, key))

#define map_set(m, key, value) \
	((m)->tmp = (value),       \
	 map_set_(&(m)->base, key, &(m)->tmp, sizeof((m)->tmp)))

#define map_remove(m, key) \
	map_remove_(&(m)->base, key)

#define map_iter(m) \
	map_iter_()

#define map_next(m, iter) \
	map_next_(&(m)->base, iter)

void map_deinit_(map_base_t *m);
void *map_get_(map_base_t *m, const char *key);
int map_set_(map_base_t *m, const char *key, void *value, int vsize);
void map_remove_(map_base_t *m, const char *key);
map_iter_t map_iter_(void);
const char *map_next_(map_base_t *m, map_iter_t *iter);

typedef map_t(void *) map_void_t;
typedef map_t(char *) map_str_t;
typedef map_t(int) map_int_t;
typedef map_t(char) map_char_t;

struct map_node_t
{
	unsigned hash;
	void *value;
	map_node_t *next;
};

static unsigned map_hash(const char *str)
{
	unsigned hash = 5381;
	while (*str)
	{
		hash = ((hash << 5) + hash) ^ *str++;
	}
	return hash;
}

static map_node_t *map_newnode(const char *key, void *value, int vsize)
{
	map_node_t *node;
	int ksize = strlen(key) + 1;
	int voffset = ksize + ((sizeof(void *) - ksize) % sizeof(void *));
	node = malloc(sizeof(*node) + voffset + vsize);
	if (!node)
		return NULL;
	memcpy(node + 1, key, ksize);
	node->hash = map_hash(key);
	node->value = ((char *)(node + 1)) + voffset;
	memcpy(node->value, value, vsize);
	return node;
}

static int map_bucketidx(map_base_t *m, unsigned hash)
{
	return hash & (m->nbuckets - 1);
}

static void map_addnode(map_base_t *m, map_node_t *node)
{
	int n = map_bucketidx(m, node->hash);
	node->next = m->buckets[n];
	m->buckets[n] = node;
}

static int map_resize(map_base_t *m, int nbuckets)
{
	map_node_t *nodes, *node, *next;
	map_node_t **buckets;
	int i;
	nodes = NULL;
	i = m->nbuckets;
	while (i--)
	{
		node = (m->buckets)[i];
		while (node)
		{
			next = node->next;
			node->next = nodes;
			nodes = node;
			node = next;
		}
	}
	/* Reset buckets */
	buckets = realloc(m->buckets, sizeof(*m->buckets) * nbuckets);
	if (buckets != NULL)
	{
		m->buckets = buckets;
		m->nbuckets = nbuckets;
	}
	if (m->buckets)
	{
		memset(m->buckets, 0, sizeof(*m->buckets) * m->nbuckets);
		/* Re-add nodes to buckets */
		node = nodes;
		while (node)
		{
			next = node->next;
			map_addnode(m, node);
			node = next;
		}
	}
	/* Return error code if realloc() failed */
	return (buckets == NULL) ? -1 : 0;
}

static map_node_t **map_getref(map_base_t *m, const char *key)
{
	unsigned hash = map_hash(key);
	map_node_t **next;
	if (m->nbuckets > 0)
	{
		next = &m->buckets[map_bucketidx(m, hash)];
		while (*next)
		{
			if ((*next)->hash == hash && !strcmp((char *)(*next + 1), key))
			{
				return next;
			}
			next = &(*next)->next;
		}
	}
	return NULL;
}

void map_deinit_(map_base_t *m)
{
	map_node_t *next, *node;
	int i;
	i = m->nbuckets;
	while (i--)
	{
		node = m->buckets[i];
		while (node)
		{
			next = node->next;
			free(node);
			node = next;
		}
	}
	free(m->buckets);
}

void *map_get_(map_base_t *m, const char *key)
{
	map_node_t **next = map_getref(m, key);
	return next ? (*next)->value : NULL;
}

int map_set_(map_base_t *m, const char *key, void *value, int vsize)
{
	int n, err;
	map_node_t **next, *node;
	next = map_getref(m, key);
	if (next)
	{
		memcpy((*next)->value, value, vsize);
		return 0;
	}
	node = map_newnode(key, value, vsize);
	if (node == NULL)
		goto fail;
	if (m->nnodes >= m->nbuckets)
	{
		n = (m->nbuckets > 0) ? (m->nbuckets << 1) : 1;
		err = map_resize(m, n);
		if (err)
			goto fail;
	}
	map_addnode(m, node);
	m->nnodes++;
	return 0;
fail:
	if (node)
		free(node);
	return -1;
}

void map_remove_(map_base_t *m, const char *key)
{
	map_node_t *node;
	map_node_t **next = map_getref(m, key);
	if (next)
	{
		node = *next;
		*next = (*next)->next;
		free(node);
		m->nnodes--;
	}
}

map_iter_t map_iter_(void)
{
	map_iter_t iter;
	iter.bucketidx = -1;
	iter.node = NULL;
	return iter;
}

const char *map_next_(map_base_t *m, map_iter_t *iter)
{
	if (iter->node)
	{
		iter->node = iter->node->next;
		if (iter->node == NULL)
			goto nextBucket;
	}
	else
	{
	nextBucket:
		do
		{
			if (++iter->bucketidx >= m->nbuckets)
			{
				return NULL;
			}
			iter->node = m->buckets[iter->bucketidx];
		} while (iter->node == NULL);
	}
	return (char *)(iter->node + 1);
}
#endif
)";

// combination
const std::string clar_library = clar_header + clar_typedef + clar_optionals +
                                 clar_lists + clar_vars + clar_funcs +
                                 clar_mapping;

}; // namespace ClarityTemplate

#endif