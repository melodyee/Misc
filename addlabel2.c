/*
 * addlabel.c
 *
 *  Created on: 2010-10-22
 *      Author: zsc
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <elf.h>
#include <sys/stat.h>
#include <sys/mman.h>

#define TRUE 1
#define FALSE 0

typedef struct _TYP {
	int addr;
	char *name;
	int name_idx;
} *TYP;

typedef struct _list {
	TYP data;
	struct _list *next;
} list;

TYP list_head(list *p) {
	return p->data;
}

list *list_cons(TYP e, list *p) {
	list *p2 = (list*) malloc(sizeof(list));
	p2->next = p;
	p2->data = e;
	return p2;
}

//list *list_tail(list *p) {
//	list *p2 = p->next;
//	free(p);
//	return p2;
//}

int list_len(list *p) {
	if (!p)
		return 0;
	return 1 + list_len(p->next);
}

char *labels () {
	static int i =0;
	char *s = (char*)malloc(20);
	sprintf(s,"xxx_label_%d",i);
	i++;
	return s;
}

list *stack = 0;

int is_branch(char* code) {
	if ((code[3]>>4)==1)
		return TRUE;
	return FALSE;
}

int is_jump(char* code) {
	//jalr
	if ((code[0] & 0x3f) == 0b001001 && (code[3] >> 3) == 0)
		return TRUE;
	return FALSE;
}

int get_section_idx_by_name (Elf32_Ehdr* ehdr, const char *name) {
	Elf32_Shdr *shdr, *shstrtab;
	int shstrtab_off;
	char *Real_strtab;
	char *in = (char*) ehdr;
	int i;

	shdr = (Elf32_Shdr *) (in + ehdr->e_shoff);
	shstrtab = &shdr[ehdr->e_shstrndx];
	shstrtab_off = (unsigned int) shstrtab->sh_offset;
	Real_strtab = (char *) (in + shstrtab_off);

	for (i = 0; i < (int) ehdr->e_shnum; i++) {
		if (strcmp(Real_strtab + shdr[i].sh_name, name) == 0) {
			return i;
		}
	}
	perror("get_section_idx_by_name");
	exit(-1);
}

Elf32_Shdr *get_section_by_name(Elf32_Ehdr* ehdr, const char *name) {
	Elf32_Shdr *shdr;
	char *in = (char*) ehdr;

	shdr = (Elf32_Shdr *) (in + ehdr->e_shoff);
	return &shdr[get_section_idx_by_name(ehdr,name)];
}

int iter_text(Elf32_Ehdr* ehdr) {
	int i;
	Elf32_Shdr *text = get_section_by_name(ehdr, ".text");
	int ninstr = text->sh_size / 4;
	int addr = text->sh_addr;
	char *in= (char*) ehdr;
	int *code = (int*)(in + text->sh_offset);
	for (i = 0; i < ninstr; i++) {
		if (is_jump((char*) code) ||is_branch ((char*) code)) {
			struct _TYP *t = (TYP)malloc(sizeof(struct _TYP));
			//printf("%x@%x\n", *(code), addr + i * 4);
			t->addr = addr + i * 4 + 8;
			t->name = labels();
			t->name_idx = 0;
			stack = list_cons (t, stack);
		}
		code++;
	}
	return 0;
}

void enlarge_section(const char* section_name, unsigned int bytes, char *in,
		char* out) {
	int i;
	Elf32_Ehdr *ehdr = (Elf32_Ehdr *) in;
	Elf32_Ehdr *ehdr2 = (Elf32_Ehdr *) out;
	Elf32_Phdr *phdr2, *phdr;
	Elf32_Shdr *shdr2, *shdr, *shstrtab;
	int shstrtab_off;
	char *Real_strtab;
	unsigned int section_off = 0;
	int section_idx = 0;

	memcpy(ehdr2, (char*) ehdr, ehdr->e_ehsize);
	shdr = (Elf32_Shdr *) (in + ehdr->e_shoff);

	shstrtab = &shdr[ehdr->e_shstrndx];
	shstrtab_off = (unsigned int) shstrtab->sh_offset;
	Real_strtab = (char *) (in + shstrtab_off);

	for (i = 0; i < (int) ehdr->e_shnum; i++) {
		if (strcmp(Real_strtab + shdr[i].sh_name, section_name) == 0) {
			section_off = shdr[i].sh_offset;
			section_idx = i;
			break;
		}
	}
	if (section_off==0) {
		perror("enlarge_section: section not found");
		exit(-1);
	}

	ehdr2->e_phoff = ehdr->e_phoff + (ehdr->e_phoff>section_off?bytes:0);
	phdr = (Elf32_Phdr *) (in + ehdr->e_phoff);
	phdr2 = (Elf32_Phdr *) (out + ehdr2->e_phoff);
	memcpy(phdr2, (char*) phdr, ehdr->e_phnum * sizeof(Elf32_Phdr));

	ehdr2->e_shoff = ehdr->e_shoff + (ehdr->e_shoff>section_off?bytes:0);

	shdr2 = (Elf32_Shdr *) (out + ehdr2->e_shoff);
	memcpy(shdr2, (char*) shdr, ehdr->e_shnum * sizeof(Elf32_Shdr));
	printf ("%d copied, shnum = %d\n",ehdr->e_shnum * sizeof(Elf32_Shdr), ehdr->e_shnum );
	shdr2[section_idx].sh_size = shdr[section_idx].sh_size + bytes;

	for (i = 0; i < (int) ehdr->e_shnum; i++) {
		if (shdr[i].sh_offset > section_off)
			shdr2[i].sh_offset = shdr[i].sh_offset + bytes;
		//printf ("%x,%x\n", shdr2[i].sh_offset,shdr2[i].sh_size);
		memcpy(out + shdr2[i].sh_offset, in + shdr[i].sh_offset,
					shdr[i].sh_size);
	}
}

int dump_to_file(char* buf, int len, const char *out_file) {
	FILE *fp = fopen(out_file, "wb");
	if (len!=fwrite(buf, len, 1, fp))
		goto err;
	fclose(fp);
	return 0;
err:
	if (fp)
		fclose(fp);
	return -1;
}

Elf32_Ehdr *open_elf(const char *elf_file) {
	int fd;
	struct stat stat;
	Elf32_Ehdr *ehdr;

	fd = open(elf_file, O_RDONLY);
	if (fd == -1) {
		perror("elf_file");
		exit(-1);
	}
	if (fstat(fd, &stat) == -1) {
		perror("fstat");
		exit(-1);
	}

	ehdr = mmap(0, stat.st_size, PROT_READ, MAP_SHARED, fd, 0);
	if (ehdr == MAP_FAILED) {
		perror("mmap ehdr");
		goto err;
	}
	return ehdr;
	err:
		if (ehdr)
			munmap(ehdr, stat.st_size);
		if (fd != -1)
			close(fd);
	return 0;
}

char *accum_string(char *p, const char *str) {
	int i;
	for(i=0;str[i];i++) {
		*p = str[i];
		p++;
	}
	*p++ = 0;
	return p;
}

int count_strings (char *p, char *lim) {
	int s = 0;
	while (p<lim) {
		if (*p==0) s+=1;
		p++;
	}
	return s;
}

int count_strings_strtab (Elf32_Ehdr *ehdr, Elf32_Shdr *strtab) {
	char *in = (char*) ehdr;
	return count_strings(in + strtab->sh_offset,in + strtab->sh_offset + strtab->sh_size);
}

void enlarge_section_file(const char *elf_file, const char *out_file, const char *section,
		int extra) {
	int fd;
	struct stat stat;
	Elf32_Ehdr *ehdr;
	char *out;
	fd = open(elf_file, O_RDONLY);
	if (fd == -1) {
		perror("elf_file");
		exit(-1);
	}

	if (fstat(fd, &stat) == -1) {
		perror("fstat");
		exit(-1);
	}
	ehdr = mmap(0, stat.st_size, PROT_READ, MAP_SHARED, fd, 0);
	if (ehdr == MAP_FAILED) {
		perror("mmap ehdr");
		goto err;
	}

	out = (char*) malloc(stat.st_size + extra);

	enlarge_section(section , extra, (char *)ehdr, out);

	dump_to_file(out, stat.st_size + extra, out_file);

err:
	if (ehdr)
		munmap(ehdr, stat.st_size);
	if (fd != -1)
		close(fd);
}

void label_file(const char *elf_file, const char *out_file) {
	int fd;
	struct stat stat;
	Elf32_Ehdr *ehdr;
	char *out, *out2;
	Elf32_Shdr *strtab, *strtab2, *symtab, *symtab2;
	int extra_symtab_bytes = 0;
	int extra_strtab_bytes = 0;
	list *p;
	char *buf;
	char *ppos;
	int stridx;
	int symtabidx;
	Elf32_Sym *new_symtab_data;
	int i;
	int text_section_idx;
	fd = open(elf_file, O_RDONLY);
	if (fd == -1) {
		perror("elf_file");
		exit(-1);
	}

	if (fstat(fd, &stat) == -1) {
		perror("fstat");
		exit(-1);
	}
	ehdr = mmap(0, stat.st_size, PROT_READ, MAP_SHARED, fd, 0);
	if (ehdr == MAP_FAILED) {
		perror("mmap ehdr");
		goto err;
	}

	strtab = get_section_by_name(ehdr, ".strtab");
	stridx = count_strings_strtab(ehdr, strtab);
	printf ("there were %d strings\n", stridx);

	iter_text(ehdr);

	for(p=stack;p;p=p->next) {
		TYP t = p->data;
		extra_strtab_bytes+=strlen(t->name)+1;
	}

	buf = (char*) malloc(extra_strtab_bytes);
	ppos = buf;

	for(p=stack;p;p=p->next) {
		TYP t = p->data;
		t->name_idx = ppos-buf + strtab->sh_size;
		//printf ("name_idx = %d\n", t->name_idx);
		ppos = accum_string(ppos,t->name);
	}

	printf ("added %d space for new strings\n", extra_strtab_bytes);

	out = (char*) malloc(stat.st_size + extra_strtab_bytes);

	enlarge_section(".strtab", extra_strtab_bytes, (char*)ehdr, out);
	printf ("enlarge_section\n");

	strtab2 = get_section_by_name((Elf32_Ehdr *)out, ".strtab");

	memcpy(out+strtab2->sh_offset+strtab->sh_size,buf,extra_strtab_bytes);

	free(buf);

	symtab = get_section_by_name((Elf32_Ehdr *)out, ".symtab");
	symtabidx = symtab->sh_size / symtab->sh_entsize;
	printf ("there were %d symtab entries\n", symtabidx);

	extra_symtab_bytes = list_len(stack) * symtab->sh_entsize;
	printf ("added %d space for new symbols\n", extra_symtab_bytes);

	new_symtab_data = (Elf32_Sym*) malloc(extra_symtab_bytes);
	text_section_idx = get_section_idx_by_name(ehdr,".text");
	printf ("text_section_idx = %d\n", text_section_idx);
	i = 0;
	for(p=stack;p;p=p->next) {
		TYP t = p->data;
		new_symtab_data [i].st_info = 0;
		new_symtab_data [i].st_other = 0;
		new_symtab_data [i].st_size = 0;
		new_symtab_data [i].st_name = t->name_idx;
		new_symtab_data [i].st_value = t->addr;
		new_symtab_data [i].st_shndx = text_section_idx;
		i++;
	}

	out2 = (char*) malloc(stat.st_size + extra_strtab_bytes + extra_symtab_bytes);
	enlarge_section(".symtab", extra_symtab_bytes, out, out2);

	symtab2 = get_section_by_name((Elf32_Ehdr *)out2, ".symtab");
	memcpy(out2+symtab2->sh_offset+symtab->sh_size,new_symtab_data,extra_symtab_bytes);

	dump_to_file(out2, stat.st_size + extra_strtab_bytes + extra_symtab_bytes, out_file);
	printf ("dump_to_file\n");

err:
	if (ehdr)
		munmap(ehdr, stat.st_size);
	if (fd != -1)
		close(fd);
}

int main(int argc, char *argv[]) {
	const char *fname = argv[1];
	const char *section = argv[2];
	int extra = atoi(argv[3]);

	//enlarge_section_file(fname, "out", section, extra);
	label_file (fname, "out");
	return 0;
}
