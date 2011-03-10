/*
 * t.c
 *
 *  Created on: 2010-3-22
 *      Author: zsc
 */

#include<stdio.h>
#include<string.h>
#include<errno.h>
#include<malloc.h>
#include<elf.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <sys/mman.h>
#include <fcntl.h>


#define bool int
#define true 1
#define false 0
#define KERN_DEBUG ""
#define printk printf
#define kmalloc(x,y) malloc(x)
#define kfree(x) free(x)
int kernel_read(FILE* fp, int offset, char* buffer, int length) {
	fseek(fp, offset, SEEK_SET);
	return fread(buffer, 1, length, fp);
}

typedef int32_t address;
typedef struct _list {
	struct _list *next;
	union {
		struct {
			address key;
			address data;
		} pair;
		int64_t phys_addr;
	} u;
} list;

bool list_is_member(address key, list* l) {
	list *p = l;
	while (p) {
		if (p->u.pair.key == key) {
			return true;
		}
		p = p->next;
	}
	return false;
}

list* list_append(list* l, int64_t phys_addr) {
	list *p;

	p = kmalloc(sizeof(list), GFP_KERNEL);
	if (!p)
		return NULL;
	p->next = l;
	p->u.phys_addr = phys_addr;
	return p;
}

list* list_append_pair(list* l, address key, address data) {
	list *p;

	p = kmalloc(sizeof(list), GFP_KERNEL);
	if (!p)
		return NULL;
	p->next = l;
	p->u.pair.key = key;
	p->u.pair.data = data;
	return p;
}

address list_find_key(list* l, address key) {
	static address last_key;
	static address last_data;
	list* p;
	if (key == last_key)
		return last_data;
	p = l;
	while (p) {
		if (p->u.pair.key == key) {
			last_key = key;
			last_data = p->u.pair.data;
			return last_data;
		}
		p = p->next;
	}
	return 0;
}

void free_list(list *l) {
	list *p = l;
	list *newp;
	while (p) {
		newp = p->next;
		kfree (p);
		p = newp;
	}
}

void print_list(list *l) {
	if (l) {
		while (l) {
			printk(KERN_DEBUG"%x:%x,", l->u.pair.key, l->u.pair.data);
			l = l->next;
		}
		printk(KERN_DEBUG"\n");
	} else
		printk(KERN_DEBUG"NULL\n");
}

#define NUM_BUCKETS_BITS 6
#define NUM_BUCKETS (1<<NUM_BUCKETS_BITS)
#define NUM_BUCKETS_MASK (NUM_BUCKETS-1)
typedef struct _htable {
	list* buckets[NUM_BUCKETS];
} htable;

htable* htable_insert(htable* h, address key, address data) {
	int n = (key >> 2) & NUM_BUCKETS_MASK;
	list* l = list_append_pair(h->buckets[n], key, data);
	if (!l) {
		printk(KERN_DEBUG"htable_insert faults\n");
		return NULL;
	}
	h->buckets[n] = l;
	return h;
}

htable* htable_create() {
	int i;
	htable* h = kmalloc(sizeof(htable), GFP_KERNEL);
	if (!h)
		return NULL;
	for (i = 0; i < NUM_BUCKETS; i++) {
		h->buckets[i] = NULL;
	}
	return h;
}

void free_htable(htable* h) {
	if (h) {
		int i;
		for (i = 0; i < NUM_BUCKETS; i++) {
			free_list(h->buckets[i]);
		}
		kfree(h);
		printk(KERN_DEBUG"free_htable\n");
	}
}

htable* htable_of_array(address* arr, int len) {
	int i;
	htable* h = htable_create();
	if (!h)
		return NULL;
	for (i = 0; i < len; i += 2) {
		if (!htable_insert(h, arr[i], arr[i + 1]))
			goto free;
	}
	return h;
free:
	free_htable(h);
	return NULL;
}

address htable_find(htable* h, address key) {
	static address last_key;
	static address last_data;
	if (!h) {
		printk(KERN_DEBUG"htable_find: null htable\n");
		return 0;
	}
	if (key == last_key)
		return last_data;
	last_key = key;
	return (last_data = list_find_key(h->buckets[(key >> 2) & NUM_BUCKETS_MASK],
			key));
}

void print_htable(htable* h) {
	int i;
	for (i = 0; i < NUM_BUCKETS; i++) {
		print_list(h->buckets[i]);
	}
	printk(KERN_DEBUG"-----\n");
}

int canEncodeInMBit(int n, int m) {
	return n<(1<<(m-1)) || n>=-(1<<(m-1));
}

static struct _htable* htable_of_file(char *fname) {
	Elf32_Ehdr _ehdr;
	Elf32_Ehdr *ehdr = &_ehdr;
	Elf32_Shdr _shdr;
	Elf32_Shdr *shdr = &_shdr;
	Elf32_Shdr _shstrtab;
	Elf32_Shdr *shstrtab = &_shstrtab;
	struct _htable* htable = NULL;
	char *shstrtab_buffer;
	address* address_array;
	address source_addr, targ_addr;
	int decoded;
	int address_array_length = 0;
	int i,j;
	void *start_fp;
	struct stat stat_data;
	int fd;
	int dist;
	FILE* file = fopen(fname, "rb");
	int *instruction;
	int textSectionBaseAddr = 0x10000000;
	int countPiggyBack=0;
	int trapTableOffset = 0;
	address* trapTableAddr;
	if (file)
		printf("parsing %s\n", fname);
	else
		goto err;

	if (kernel_read(file, 0, (char *) ehdr, sizeof(Elf32_Ehdr))
			!= sizeof(Elf32_Ehdr))
		goto err;
	if (ehdr->e_ident[EI_MAG0] != 0x7f
			|| ehdr->e_ident[EI_MAG1] != 'E'
			|| ehdr->e_ident[EI_MAG2] != 'L'
			|| ehdr->e_ident[EI_MAG3] != 'F'
			|| ehdr->e_ident[EI_CLASS] != ELFCLASS32
			|| ehdr->e_ident[EI_DATA] != ELFDATA2LSB
			|| ehdr->e_ident[EI_VERSION] != EV_CURRENT
			|| ehdr->e_type != ET_EXEC
			|| ehdr->e_version != EV_CURRENT) {
		printk(KERN_DEBUG"File type not supported\n");
		goto err;
	}
	if (kernel_read(file,
			ehdr->e_shoff + ehdr->e_shstrndx * sizeof(Elf32_Shdr),
			(char *) shstrtab, sizeof(Elf32_Shdr)) != sizeof(Elf32_Shdr))
		goto err;
	shstrtab_buffer = kmalloc(1024, GFP_KERNEL);
	if (!shstrtab_buffer)
		goto err;
	if (kernel_read(file, shstrtab->sh_offset, (char *) shstrtab_buffer,
			shstrtab->sh_size) != shstrtab->sh_size)
		goto free;
	for (i = 0; i < (int) ehdr->e_shnum; i++) {
		if (kernel_read(file, ehdr->e_shoff + i * sizeof(Elf32_Shdr),
				(char *) shdr, sizeof(Elf32_Shdr)) != sizeof(Elf32_Shdr))
			goto free;
		if (strcmp(shstrtab_buffer + shdr->sh_name, ".TRAPTABLE") == 0) {
			printk(KERN_DEBUG"[+] .TRAPTABLE Off: 0x%x size:0x%x\n",
					(unsigned int) shdr->sh_offset, shdr->sh_size);
			trapTableOffset = shdr->sh_offset;
			address_array = kmalloc(shdr->sh_size, GFP_KERNEL);
			if (kernel_read(file, shdr->sh_offset, (char *) address_array,
					shdr->sh_size) != shdr->sh_size) {
				kfree(address_array);
				goto free;
			}
			address_array_length = shdr->sh_size/ sizeof(int32_t);
			htable = htable_of_array(address_array, shdr->sh_size
					/ sizeof(int32_t));
			//kfree(address_array);
			break;
		}
	}
	for (i = 0; i < (int) ehdr->e_shnum; i++) {
		if (kernel_read(file, ehdr->e_shoff + i * sizeof(Elf32_Shdr),
				(char *) shdr, sizeof(Elf32_Shdr)) != sizeof(Elf32_Shdr))
			goto free;
		if (strcmp(shstrtab_buffer + shdr->sh_name, ".text") == 0) {
			printk(KERN_DEBUG"[+] .text Off: 0x%x size:0x%x\n",
					(unsigned int) shdr->sh_offset, shdr->sh_size);
			fclose(file);
			fd = open (fname, O_RDWR);
			if( (fstat(fd,&stat_data) ) < 0 )
			{
			printk(" fstat error !\n");
			goto free;
			}
			if( (start_fp = mmap(NULL,stat_data.st_size,
					PROT_WRITE,MAP_SHARED,fd,0 )) == MAP_FAILED)
			{
				perror("mmap");
			printk(" mmap error !\n");
			goto free;
			}
			trapTableAddr = &start_fp[trapTableOffset];
			for(j=0;j<address_array_length/2;j++) {
				source_addr = address_array[2*j];
				targ_addr = address_array[2*j+1];
				if (!source_addr || !targ_addr)
					continue;
				dist = (targ_addr-source_addr)/4;
				instruction = &start_fp[source_addr-textSectionBaseAddr];
				//teq allows encoding 10 bit
				if ((dist<=511 && dist>=-511)
						&& !((*instruction)&0xfc000000) // immediate version has no code field
						) {
					countPiggyBack ++;
					printk("source_addr = 0x%x\n", source_addr);
					printk("From 0x%x\n", *instruction);
					*instruction = *instruction & (~(1023<<6));
					*instruction = (*instruction) | ((dist+512)<<6);
					printk("To 0x%x\n", *instruction);
					decoded = ((*instruction >> 6) & 0x3ff) - 512;
					printk("dist = %d, 4*decoded = %d, targ_addr - source_addr = %d\n",
							dist, 4*decoded, targ_addr - source_addr );
#ifdef SHRINK_TABLE
					trapTableAddr[2*j] = 0;
					trapTableAddr[2*j+1] = 0;
#endif
				}
			}

			close(fd);
			munmap(start_fp, stat_data.st_size);
			printk("performed %d PiggyBack out of %d\n", countPiggyBack, address_array_length/2);
			break;
		}
	}
free:
	kfree(shstrtab_buffer);
err:
	return htable;
}

address arr[] = { 0x1122, 0x2244, 0x1122, 0x2244, 0x3355, 0x6688, 0x4355,
		0x6688 };
int main(int argc, char** argv) {
	htable * h;
	FILE *fp;
#if 0
	h = htable_of_array (arr,sizeof(arr)/sizeof(address));
#else
	h = htable_of_file(argv[1]);
#endif
	//print_htable(h);
	return 0;
}
