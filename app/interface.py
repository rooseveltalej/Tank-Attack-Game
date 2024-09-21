import pygame
import random

# Inicialización de Pygame
pygame.init()

# Configuración de pantalla
SCREEN_WIDTH = 800
SCREEN_HEIGHT = 600
screen = pygame.display.set_mode((SCREEN_WIDTH, SCREEN_HEIGHT))
pygame.display.set_caption("Juego de Tanques")

# Colores
BLACK = (0, 0, 0)
WHITE = (255, 255, 255)

# Tamaño de los bloques (tanques, muros)
BLOCK_SIZE = 40

# Cargar imágenes
player_image = pygame.image.load('images/tank_player.png')
enemy_image = pygame.image.load('images/tank_enemy.png')
wall_image = pygame.image.load('images/wall.png')

# Escalar imágenes si es necesario
player_image = pygame.transform.scale(player_image, (BLOCK_SIZE, BLOCK_SIZE))
enemy_image = pygame.transform.scale(enemy_image, (BLOCK_SIZE, BLOCK_SIZE))
wall_image = pygame.transform.scale(wall_image, (BLOCK_SIZE, BLOCK_SIZE))

clock = pygame.time.Clock()

# El juego soporta una matriz de 20x15 bloques
MAP = [
    [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1],
    [1, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 1, 1, 1],
    [1, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1],
    [1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1],
    [1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1],
    [1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1],
    [1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1],
    [1, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1],
    [1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1],
    [1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1],
    [1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1],
    [1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1],
    [1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1],
    [1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1],
    [1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1],
]


# --- CLASES ---

class Tank:
    def __init__(self, x, y, image, speed=5):  # Agregar parámetro speed
        self.rect = pygame.Rect(x, y, BLOCK_SIZE, BLOCK_SIZE)
        self.image = image  # Usamos la imagen en lugar del color
        self.original_image = image  # Guardar la imagen original para rotarla después
        self.speed = speed  # Atributo de velocidad
        self.direction = 'UP'  # Dirección inicial

    def draw(self, screen):
        # Dibujar la imagen en lugar del rectángulo
        screen.blit(self.image, self.rect)

    def move(self, x_change, y_change, walls):
        # Nueva posición
        new_rect = self.rect.move(x_change, y_change)

        # Verificar colisión con muros
        for wall in walls:
            if new_rect.colliderect(wall.rect):
                return  # No moverse si choca con un muro

        self.rect = new_rect

    def rotate_image(self):
        """ Rota la imagen del tanque según la dirección """
        if self.direction == 'UP':
            self.image = pygame.transform.rotate(self.original_image, 0)  # Imagen original
        elif self.direction == 'DOWN':
            self.image = pygame.transform.rotate(self.original_image, 180)
        elif self.direction == 'LEFT':
            self.image = pygame.transform.rotate(self.original_image, 90)
        elif self.direction == 'RIGHT':
            self.image = pygame.transform.rotate(self.original_image, -90)

class Wall:
    def __init__(self, x, y):
        self.rect = pygame.Rect(x, y, BLOCK_SIZE, BLOCK_SIZE)

    def draw(self, screen):
        # Dibujar la imagen del muro
        screen.blit(wall_image, self.rect)


class EnemyTank(Tank):
    def update(self, player_pos, walls):
        # Movimiento básico hacia el jugador
        if self.rect.x < player_pos[0]:
            x_change = 5
        elif self.rect.x > player_pos[0]:
            x_change = -5
        else:
            x_change = 0

        if self.rect.y < player_pos[1]:
            y_change = 5
        elif self.rect.y > player_pos[1]:
            y_change = -5
        else:
            y_change = 0

        self.move(x_change, y_change, walls)


class Bullet:
    def __init__(self, x, y, direction):
        self.rect = pygame.Rect(x, y, 5, 5)
        self.speed = 10
        self.direction = direction  # ('UP', 'DOWN', 'LEFT', 'RIGHT')

    def move(self):
        if self.direction == 'UP':
            self.rect.y -= self.speed
        elif self.direction == 'DOWN':
            self.rect.y += self.speed
        elif self.direction == 'LEFT':
            self.rect.x -= self.speed
        elif self.direction == 'RIGHT':
            self.rect.x += self.speed

    def draw(self, screen):
        pygame.draw.rect(screen, WHITE, self.rect)

    def off_screen(self):
        return (self.rect.x < 0 or self.rect.x > SCREEN_WIDTH or 
                self.rect.y < 0 or self.rect.y > SCREEN_HEIGHT)


# --- FUNCIONES ---
def generate_walls(map_data):
    """Generar muros según el mapa"""
    walls = []
    for row in range(len(map_data)):
        for col in range(len(map_data[row])):
            if map_data[row][col] == 1:
                wall = Wall(col * BLOCK_SIZE, row * BLOCK_SIZE)
                walls.append(wall)
    return walls

walls = generate_walls(MAP)


# --- FUNCIONES AUXILIARES ---

def handle_bullets(bullets, enemies):
    """ Mueve las balas y verifica colisiones con los enemigos """
    for bullet in bullets[:]:  # Usamos [:] para copiar la lista mientras la iteramos
        bullet.move()

        # Verificar si una bala golpea a un enemigo
        for enemy in enemies[:]:  # También hacemos copia de enemigos para eliminar mientras iteramos
            if bullet.rect.colliderect(enemy.rect):
                enemies.remove(enemy)  # Eliminar enemigo
                bullets.remove(bullet)  # Eliminar bala
                break

        # Eliminar la bala si sale de la pantalla
        if bullet.off_screen():
            bullets.remove(bullet)


# --- CONFIGURACIÓN INICIAL ---

player = Tank(100, 40, player_image)  # Tanque del jugador
#walls = [Wall(200, 200), Wall(240, 200)]  # Lista de muros
enemies = [EnemyTank(400, 400, enemy_image), EnemyTank(600, 400, enemy_image)]  # Tanques enemigos
bullets = []  # Lista de balas

# --- BUCLE PRINCIPAL ---

running = True
while running:
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            running = False

        # Disparar bala al presionar la barra espaciadora
        if event.type == pygame.KEYDOWN:
            if event.key == pygame.K_SPACE:
                # Crear una bala en la dirección actual del jugador
                if keys[pygame.K_UP]:
                    bullet = Bullet(player.rect.centerx, player.rect.top, 'UP')
                elif keys[pygame.K_DOWN]:
                    bullet = Bullet(player.rect.centerx, player.rect.bottom, 'DOWN')
                elif keys[pygame.K_LEFT]:
                    bullet = Bullet(player.rect.left, player.rect.centery, 'LEFT')
                elif keys[pygame.K_RIGHT]:
                    bullet = Bullet(player.rect.right, player.rect.centery, 'RIGHT')
                bullets.append(bullet)

    # Manejo del movimiento del tanque del jugador
    keys = pygame.key.get_pressed()
    x_change, y_change = 0, 0

    if keys[pygame.K_LEFT]:
        x_change = -player.speed
        player.direction = 'LEFT'  # Actualizar dirección
    if keys[pygame.K_RIGHT]:
        x_change = player.speed
        player.direction = 'RIGHT'  # Actualizar dirección
    if keys[pygame.K_UP]:
        y_change = -player.speed
        player.direction = 'UP'  # Actualizar dirección
    if keys[pygame.K_DOWN]:
        y_change = player.speed
        player.direction = 'DOWN'  # Actualizar dirección

    player.move(x_change, y_change, walls)
    player.rotate_image()  # Rotar la imagen según la dirección

    # Actualizar y mover los enemigos hacia el jugador
    for enemy in enemies:
        enemy.update(player.rect.topleft, walls)

    # Manejo de balas
    handle_bullets(bullets, enemies)

    # Dibujar los elementos en la pantalla
    screen.fill(BLACK)
    player.draw(screen)
    

    for enemy in enemies:
        enemy.draw(screen)

    for bullet in bullets:
        bullet.draw(screen)

    for wall in walls:
        wall.draw(screen)

    pygame.display.flip()
    clock.tick(30)  # 30 FPS

pygame.quit()
